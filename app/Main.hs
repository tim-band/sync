module Main where

import System.FilePath (FilePath, isPathSeparator)
import Transformer (parseValue)
import Data.Yaml (decodeFileThrow, Value, parseEither)
import System.IO (putStrLn)
import Control.Monad (forM_)

main :: IO ()
main = do
    v <- decodeFileThrow "example.yaml"
    case parseEither parseValue v of
        Left msg -> putStrLn ("Parse error: " ++ msg)
        Right tr -> do
            ps <- tr "/"
            forM_ ps $ \p -> do
                putStrLn p
