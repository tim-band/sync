module Directories (name, parser) where

import Control.Exception (handle)
import Control.Monad (forM, filterM)
import Control.Monad.Trans.Class (lift)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import qualified Data.Yaml
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>))

import Log (logInfo)
import PathFinder (Output, PathFinderO, chain)

name = "directories"

inform :: Output -> Output
inform iofps = do
    fps <- iofps
    logInfo ("directories:\n" ++ intercalate "\n" fps)
    iofps

(.!=) :: (Functor f) => f (Maybe a) -> a -> f a
(.!=) pma d = fmap (fromMaybe d) pma
(.:?) ob s = (Data.Yaml..:?) ob (pack s)

parser :: PathFinderO
parser _ ob = doDirectories'
        <$> ob .:? "min-depth" .!= 1
        <*> ob .:? "max-depth" where
    doDirectories' minD (Just maxD) input = inform $ doDirectories minD maxD input
    doDirectories' minD Nothing input = inform $ doDirectories minD minD input
    doDirectories :: Int -> Int  -> FilePath -> Output
    doDirectories minD maxD input
        | minD <= 0 = do
            dirs <- doDirectories 1 maxD input
            return $ input : dirs
        | maxD <= 0 = return []
        | otherwise = do
            isDir <- lift $ doesDirectoryExist input
            if isDir then do
                ds <- lift $ handle ((\_ -> return []) :: IOError -> IO [FilePath]) (listDirectory input)
                dss <- forM ds $ \d -> doDirectories (minD - 1) (maxD - 1) (input </> d)
                return $ concat dss
            else return []
