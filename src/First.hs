module First (name, parser) where

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
import PathFinder (PathFinderO, PathFinder, chain)

name = "first"

(.:) ob s = (Data.Yaml..:) ob (pack s)

parser :: PathFinderO
parser tr ob = do
    ofv <- ob .: "of"
    doFirstV ofv where
        doFirstV :: Data.Yaml.Value -> Data.Yaml.Parser PathFinder
        doFirstV (Data.Yaml.Array arv) = doFirstL $ Vec.toList arv
        doFirstV o = doFirstL [o]
        doFirstL :: [Data.Yaml.Value] -> Data.Yaml.Parser PathFinder
        doFirstL vs = do
            pfs <- mapM tr vs
            return $ doFirst pfs
        doFirst :: [FilePath -> IO [FilePath]] -> FilePath -> IO [FilePath]
        doFirst [] _ = return []
        doFirst (pf:pfs) input = do
            rs <- pf input
            case rs of
                [] -> doFirst pfs input
                (r:_) -> do
                    logInfo $ "First chosen: " ++ r
                    return [r]
