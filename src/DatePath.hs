module DatePath (name, parser) where

import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Yaml
import TransformerParser (PathFinderO, chain)
import System.Directory (getModificationTime)
import System.FilePath (takeFileName, (</>))

name = "date"

(.:) ob s = (Data.Yaml..:) ob (pack s)
(.:?) ob s = (Data.Yaml..:?) ob (pack s)
(.!=) :: (Functor f) => f (Maybe a) -> a -> f a
(.!=) pma d = fmap (fromMaybe d) pma

parser :: PathFinderO
parser _ ob = doDatePath
        <$> ((ob .:? "format") .!= "") where
    doDatePath :: String -> FilePath -> IO [FilePath]
    doDatePath format input = do
        utc <- getModificationTime input
        let p = formatTime defaultTimeLocale format utc
        return [p </> takeFileName input]
