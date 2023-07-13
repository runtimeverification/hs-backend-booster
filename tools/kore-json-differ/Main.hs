-- | (c) FIXME

module Main where

import Data.Aeson.Diff as Json
import Data.Aeson as Json
import Data.Aeson.Encode.Pretty as Json
import Data.ByteString.Lazy.Char8 qualified as BS
import System.Environment

import Kore.JsonRpc.Types

usage = unlines
    [ "Display differences between two json files containing kore-rpc data"
    , ""
    , "Usage:"
    , "       <program-name> KOREJSON1 KOREJSON2"
    , ""
    , "where KOREJSON<N> are paths to files containing a kore-rpc JSON request"
    , "a kore-rpc JSON response, or a kore term in JSON format."
    ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn usage
        [x, y] -> diffJson x y
        other -> putStrLn $ "ERROR: program requires exactly two arguments.\n\n" <> usage


diffJson :: FilePath -> FilePath -> IO ()
diffJson korefile1 korefile2 = do
    contents1 <-
        Json.eitherDecode @Json.Value <$> BS.readFile korefile1
        -- FIXME a helper should iterate over possible kore-json data
        -- to determine and return the type (or return "garbled data")
    contents2 <-
        Json.eitherDecode @Json.Value <$> BS.readFile korefile2

    case (contents1, contents2) of
        (Left msg1, Left msg2) ->
            putStrLn . unlines $
                [ "Both files contain garbled json."
                , "File " <> korefile1 <> ":"
                , indent 4 msg1
                , "File " <> korefile2 <> ":"
                , indent 4 msg2
                ]
        (Left msg1, _) ->
            putStrLn . unlines $
                [ "File " <> korefile1 <> " contains garbled json."
                , indent 4 msg1
                ]
        (_, Left msg2) ->
            putStrLn . unlines $
                [ "File " <> korefile2 <> " contains garbled json."
                , indent 4 msg2
                ]
        (Right v1, Right v2) -> do
            let result = Json.diff v1 v2
            BS.putStrLn $ Json.encodePretty result

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' <>) . lines
