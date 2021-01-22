module Delete (name, parser) where

import Control.Monad.Trans.Class (lift)
import Data.Text (pack)
import Data.Yaml (Parser, Value, (.:?))
import System.Directory (removeFile, removeDirectoryRecursive)

import Log (logInfo, logWarning)
import PathFinder (PathFinderO, PathFinder, chain)

name :: [Char]
name = "delete"

parser :: PathFinderO
parser trans ob = do
  fv <- ob .:? pack "files"
  dv <- ob .:? pack "directories"
  rmMV fv dv where
    rmMV :: Maybe Value -> Maybe Value -> Parser PathFinder
    rmMV Nothing Nothing = return $ \_ -> return []
    rmMV (Just fv) Nothing = rmF <$> trans fv
    rmMV Nothing (Just dv) = rmD <$> trans dv
    rmMV (Just fv) (Just dv) = do
      f <- trans fv
      d <- trans dv
      return $ \i ->
        (++) <$> rmF f i <*> rmD d i
    rmF :: PathFinder -> PathFinder
    rmF f inp1 = f inp1 `chain` \inp2 -> do
      logInfo $ "removing file: " ++ inp2
      lift $ removeFile inp2
      return []
    rmD :: PathFinder -> PathFinder
    rmD d inp1 = d inp1 `chain` \inp2 -> do
      logInfo $ "removing directory: " ++ inp2
      lift $ removeDirectoryRecursive inp2
      return []
