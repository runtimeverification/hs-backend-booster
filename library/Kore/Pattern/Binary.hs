{-# LANGUAGE PatternSynonyms #-}

module Kore.Pattern.Binary where

import Data.Binary.Get
import Control.Monad (unless)
import Data.Int (Int16)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Bits
import Debug.Trace
import Data.Word (Word64)
import qualified Data.Map as Map
import Kore.Pattern.Base
import Control.Monad.Extra (forM)
import Kore.Syntax.ParsedKore
import Kore.Definition.Base
import GHC.Word (Word8)


pattern KORECompositePattern, KOREStringPattern, KORECompositeSort, KORESortVariable, KORESymbol, KOREVariablePattern, KOREVariable :: Word8
pattern KORECompositePattern = 0x4
pattern KOREStringPattern = 0x5
pattern KORECompositeSort = 0x6
pattern KORESortVariable = 0x7
pattern KORESymbol = 0x8
pattern KOREVariablePattern = 0x9
pattern KOREVariable = 0xD

data Version = Version {
  major :: Int16,
  minor :: Int16,
  patch :: Int16
} deriving Show

areCompatible :: Version -> Version -> Bool
areCompatible a b = a.major == b.major && a.minor == b.minor

decodeMagicHeaderAndVersion :: Get Version
decodeMagicHeaderAndVersion = do
  _ <- getWord8
  header <- getByteString 4
  unless (header == "KORE") $ fail "Invalid magic header for binary KORE"
  Version <$> getInt16le <*> getInt16le <*> getInt16le


decodeLength :: Version -> Int -> Get Int
decodeLength v l = 
  if areCompatible v $ Version 1 0 0
    then
      readAndShift l 0x0 
    else
      readAndShiftV2 True 0 0
  where
    readAndShift :: Int -> Int -> Get Int
    readAndShift counter ret
     | counter > 0 = do
        r <- getWord8
        readAndShift (counter-1) $ (ret `shiftL` 8) .|. fromIntegral r
     | otherwise = pure ret

    readAndShiftV2 :: Bool -> Int -> Int -> Get Int
    readAndShiftV2 _ steps _ | steps >= 9 = fail "No terminating byte in variable-length field"
    readAndShiftV2 False _ ret = pure ret
    readAndShiftV2 True steps ret = do
      chunk <- getWord8
      let 
          contBit = 0x80
          continue = chunk .&. contBit == 0x1
          chunk' = chunk .&. complement contBit
          ret' = ret .|. (fromIntegral $ (fromIntegral chunk' :: Word64) `shiftL` (7 * steps))
      readAndShiftV2 continue (steps+1) ret'

      
decodeString :: Version -> Map.Map Int BS.ByteString -> Get (Int, BS.ByteString)
decodeString v m = getWord8 >>= \case
    0x1 -> do
      position <- fromIntegral <$> bytesRead
      len <- decodeLength v 4
      (position,) <$> getByteString len
    0x2 -> do
      position <- fromIntegral <$> bytesRead
      backref <- decodeLength v 4
      case Map.lookup (position - backref + 1) m of
        Just str -> pure (position, str)
        Nothing -> trace ("position: " <> show (position - backref) <> " map: " <> show m) $ fail "Incorrect offset for interned string"
    _ -> fail "Incorrect String encoding"

decodeBlock :: Version -> KoreDefinition -> Map.Map Int BS.ByteString -> Maybe (Symbol, [Sort]) -> [Sort] -> [Either Text.Text Term] -> Get [Either Text.Text Term]
decodeBlock v def@KoreDefinition{symbols} stringMap mSymbol sortStack termStack = do
  isEmpty >>= \empty -> if empty
    then pure termStack
    else do
      (newStringMap, nextSymbol, nextSortStack, nextTermStack) <- getWord8 >>= \case
        KORECompositePattern -> case mSymbol of
          Just symbolAndSorts -> do
            arity <- decodeLength v 2
            unless (length termStack >= arity) $ fail "Incorrect arity Term"
            term <- mkSymbolApplication symbolAndSorts $ reverse $ take arity termStack
            pure (stringMap, Nothing, sortStack, term : drop arity termStack )
          Nothing -> fail "no symbol found"
        KOREStringPattern -> do
          (pos, str) <- decodeString v stringMap
          pure (Map.insert pos str stringMap, mSymbol,sortStack, Left (Text.decodeUtf8 str) : termStack)
        KORECompositeSort -> do
          arity <- decodeLength v 2
          (pos, sortName) <- decodeString v stringMap
          unless (length sortStack >= arity) $ fail "Incorrect arity Sort"
          pure (Map.insert pos sortName stringMap, mSymbol, (SortApp (Text.decodeUtf8 sortName) $ reverse $ take arity sortStack) : drop arity sortStack, termStack)
        KORESortVariable -> fail "KORESortVariable header decoding undefined"
        KORESymbol -> do
          arity <- decodeLength v 2
          (pos, symbolName) <- decodeString v stringMap
          unless (length sortStack >= arity) $ fail "Incorrect arity KORESymbol"
          symbol <- mkSymbol (Text.decodeUtf8 symbolName) $ reverse $ take arity sortStack
          pure (Map.insert pos symbolName stringMap, Just symbol, drop arity sortStack, termStack)
        KOREVariablePattern -> fail "KOREVariablePattern header decoding undefined"
        KOREVariable -> fail "KOREVariable header decoding undefined"
        _ -> fail "Invalid header"
      decodeBlock v def newStringMap nextSymbol nextSortStack nextTermStack

  where
    mkSymbolApplication (DV sort, _) [Left txt] = pure $ Right $ DomainValue sort txt
    mkSymbolApplication (symbol, sorts) xs = do
      args <- forM xs $ \case
        Left _txt -> fail "Expecting term"
        Right trm -> pure trm
      pure $ Right $ SymbolApplication symbol sorts args


    mkSymbol "\\dv" [sort] = pure (Symbol "\\dv" [] [] sort undefined, [])
    mkSymbol name sorts = case Map.lookup name symbols of
      Just symbol@Symbol{sortVars} -> pure (symbol, zipWith (const id) sortVars sorts)
      Nothing -> fail $ "Unknown symbol " <> show name
      
decodeKorePattern :: KoreDefinition -> Get Term
decodeKorePattern def = do
  version <- decodeMagicHeaderAndVersion
  decodeBlock version def mempty Nothing [] [] >>= \case
    [Right trm] -> pure trm
    _ -> fail "Was expecting a single term"

test :: FilePath -> Text.Text -> FilePath -> IO Term
test definitionFile mainModuleName f = do
  internalModule <-
      either (error . show) id <$>
        loadDefinition mainModuleName definitionFile
  runGet (decodeKorePattern internalModule) <$> BL.readFile f