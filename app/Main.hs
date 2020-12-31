{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad (forM_)
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Map.Strict (empty)
import Data.Yaml (decodeFileThrow, Value, parseEither)
import System.Console.CmdArgs.Implicit (cmdArgs, (&=), opt, program, summary, typFile, help, Data, Typeable)
import System.FilePath (FilePath, isPathSeparator)
import System.IO (putStrLn)
import Transformer (parseValue)

newtype SyncArgs = SyncArgs
  { config :: String
  } deriving (Show, Data, Typeable)

syncArgs = SyncArgs
  { config = "example.yaml"
    &= help "The yaml file controlling what gets synced and how"
  } &= program "sync"
    &= summary "Sync version 0.1 (c) Tim Band"

main :: IO ()
main = do
    args <- cmdArgs syncArgs
    v <- decodeFileThrow $ config args
    case parseEither parseValue v of
        Left msg -> putStrLn ("Parse error: " ++ msg)
        Right tr -> do
            ps <- evalStateT (tr "/") empty
            forM_ ps $ \p -> do
                putStrLn p
