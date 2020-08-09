module Path (name, parser) where

import Data.Text (pack)
import Data.Yaml ((.:))
import TransformerParser (PathFinderO)

name = "path"

parser :: PathFinderO
parser _ ob = path <$> ob .: (pack "is") where
    path :: String -> FilePath -> IO [FilePath]
    path p _ = return [p]
