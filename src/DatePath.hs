module DatePath (name, parser) where

import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Time.Format (formatTime, defaultTimeLocale, FormatTime)
import qualified Data.Yaml

import Control.Monad.Trans.Class (lift)
import Graphics.HsExif (parseFileExif, getDateTimeOriginal)
import System.Directory (getModificationTime)
import System.FilePath (takeFileName, (</>))

import PathFinder (Output, PathFinderO, chain)

name = "date"

(.:) ob s = (Data.Yaml..:) ob (pack s)
(.:?) ob s = (Data.Yaml..:?) ob (pack s)
(.!=) :: (Functor f) => f (Maybe a) -> a -> f a
(.!=) pma d = fmap (fromMaybe d) pma

modificationTimeFormat :: FilePath -> String -> IO String
modificationTimeFormat f format = do
    utc <- getModificationTime f
    return $ formatTime defaultTimeLocale format utc

exifTimeFormat :: FilePath -> String -> IO String
exifTimeFormat f format = do
    eexif <- parseFileExif f
    case eexif of
        Right exif -> case getDateTimeOriginal exif of
            Nothing -> modificationTimeFormat f format
            Just utc -> return $ formatTime defaultTimeLocale format utc
        Left _ -> modificationTimeFormat f format

parser :: PathFinderO
parser _ ob = doDatePath
        <$> ((ob .:? "format") .!= "")
        <*> ((ob .:? "when") .!= "modification") where
    doDatePath :: String -> String -> FilePath -> Output
    doDatePath format when input = let
        getFormat :: FilePath -> String -> IO String
        getFormat = if when == "exif" then modificationTimeFormat else exifTimeFormat
        in do
            formatted <- (lift . getFormat input) format
            return [formatted </> takeFileName input]
