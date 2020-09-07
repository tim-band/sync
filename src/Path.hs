module Path (name, parser) where

import Data.Text (pack)
import Data.Yaml ((.:))
import System.Directory (makeAbsolute)
import System.FilePath (isAbsolute, (</>))

import Log (logInfo)
import PathFinder (PathFinderO)

name = "path"

parser :: PathFinderO
parser _ ob = path <$> ob .: (pack "is") where
    path :: String -> FilePath -> IO [FilePath]
    path p input = if isAbsolute p
        then do
            logInfo $ "path: " ++ p
            return [p]
        else do
            let ap = input </> p
            logInfo $ "path: " ++ ap
            return [ap]
