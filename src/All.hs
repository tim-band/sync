module All (name, parser) where

import qualified Data.Yaml
import Data.Aeson.Types (parseFail)
import Data.List.Extra (split)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import qualified Data.Vector as Vec
import System.Directory (createDirectoryIfMissing, copyFile, doesPathExist, doesDirectoryExist, doesFileExist)
import System.FilePath (takeFileName)
import System.Posix.Directory (changeWorkingDirectory)

import Log (logInfo)
import TransformerParser (PathFinderO, PathFinder, chain)

name = "all"

(.:) ob s = (Data.Yaml..:) ob (pack s)

parser :: PathFinderO
parser tr ob = do
    ofv <- ob .: "of"
    doAllV ofv where
        doAllV :: Data.Yaml.Value -> Data.Yaml.Parser PathFinder
        doAllV (Data.Yaml.Array arv) = doAllL $ Vec.toList arv
        doAllV o = doAllL [o]
        doAllL :: [Data.Yaml.Value] -> Data.Yaml.Parser PathFinder
        doAllL vs = do
            pfs <- mapM tr vs
            return $ doAll pfs
        doAll :: [FilePath -> IO [FilePath]] -> FilePath -> IO [FilePath]
        doAll pfs input = do
            rss <- mapM (\pf -> pf input) pfs
            return $ concat rss
