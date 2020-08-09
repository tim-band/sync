module Directories (name, parser) where

import Control.Monad (forM)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import qualified Data.Yaml
import TransformerParser (PathFinderO, chain)
import System.Directory (listDirectory)
import System.FilePath ((</>))

name = "directories"

(.!=) :: (Functor f) => f (Maybe a) -> a -> f a
(.!=) pma d = fmap (fromMaybe d) pma
(.:?) ob s = (Data.Yaml..:?) ob (pack s)

parser :: PathFinderO
parser _ ob = doDirectories'
        <$> ob .:? "min-depth" .!= 1
        <*> ob .:? "max-depth" where
    doDirectories' minD (Just maxD) = doDirectories minD maxD
    doDirectories' minD Nothing = doDirectories minD minD
    doDirectories :: Int -> Int  -> FilePath -> IO [FilePath]
    doDirectories minD maxD input
        | minD <= 0 = do
            dirs <- doDirectories 1 maxD input
            return $ input : dirs
        | maxD <= 0 = return []
        | otherwise = do
            ds <- listDirectory input
            dss <- forM ds $ \d -> doDirectories (minD - 1) (maxD - 1) (input </> d)
            return $ concat dss
