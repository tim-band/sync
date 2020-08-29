module Path (name, parser) where

import Data.Text (pack)
import Data.Yaml ((.:))
import TransformerParser (PathFinderO)
import System.Directory (makeAbsolute)
import Log (logInfo)

name = "path"

parser :: PathFinderO
parser _ ob = path <$> ob .: (pack "is") where
    path :: String -> FilePath -> IO [FilePath]
    path p _ = do
        ap <- makeAbsolute p
        logInfo $ "path: " ++ ap
        return [ap]
