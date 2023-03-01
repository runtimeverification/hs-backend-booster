module Count where

-- import Data.ByteString.Char8 qualified as BS
import Data.Bifunctor as B
import Data.Map qualified as Map
import Data.List
import Data.Ord
import Text.Printf

main = getContents >>=
       mapM_ (uncurry $ printf "| %s | %-3d|\n")
       . map (B.first $ takeWhile (/=':') . drop 27 . show)
       . sortBy (comparing snd)
       . Map.toList
       .  Map.fromListWith (+)
       . map (,1 :: Int)
       . lines
