module Id (name, parser) where

import Data.Text (pack)
import Data.Yaml ((.:))
import System.Directory (makeAbsolute)
import System.FilePath (takeFileName, (</>))

import Log (logInfo)
import PathFinder (Output, PathFinderO)

name :: String
name = "id"

parser :: PathFinderO
parser _ _ = return $ \input -> return [input]
