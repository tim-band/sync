module Filter (name, parser) where

import qualified Data.Yaml
import Data.List.Extra (split)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import TransformerParser (PathFinderO, chain)
import System.Directory (createDirectoryIfMissing, copyFile, doesPathExist, doesDirectoryExist, doesFileExist)
import System.FilePath (takeFileName)
import System.Posix.Directory (changeWorkingDirectory)

name = "filter"

(.:) ob s = (Data.Yaml..:) ob (pack s)
(.:?) ob s = (Data.Yaml..:?) ob (pack s)
(.!=) :: (Functor f) => f (Maybe a) -> a -> f a
(.!=) pma d = fmap (fromMaybe d) pma

-- match "*" ++ pattern against candidate
matchesStar :: String -> String -> Bool
matchesStar pattern candidate@(_:candidate') = ms pattern candidate where
    ms (p:ps) cs@(c:cs')
        | p == '*' = matchesStar ps cs
        | p == '?' = ms ps cs'
        | p == c = ms ps cs'
        | otherwise = matchesStar pattern candidate'
    ms ps@(_:_) [] = matches ps []
    ms [] (_:_) = ms pattern (drop (length candidate - length pattern) candidate)
    ms [] [] = True
matchesStar pattern [] = matches pattern []

-- match pattern against candidate
matches :: String -> String -> Bool
matches pattern@(p:ps) candidate@(c:cs)
    | p == '*' = matchesStar ps candidate
    | p == c = matches ps cs
    | otherwise = False
matches [] (_:_) = False
matches (p:ps) []
    | p == '*' = matches ps []
    | otherwise = False
match [] []  = True

parser :: PathFinderO
parser _ ob = doFilter
        <$> ((ob .:? "pattern") .!= "*")
        <*> ((ob .:? "type") .!= "?") where
    doFilter :: String -> String -> FilePath -> IO [FilePath]
    doFilter pattern ftype input = case matchesPattern pattern input of
        True -> do
            typeOk <- matchesType ftype input
            if typeOk then return [input] else return []
        False -> return []
    matchesPattern :: String -> FilePath -> Bool
    matchesPattern pattern input = let
        patternBits = split (== '*') pattern
        fn = takeFileName input in
            matches pattern fn
    matchesType :: String -> FilePath -> IO Bool
    matchesType t input
        | t == "?" = return True
        | t == "f" = doesFileExist input
        | t == "d" = doesDirectoryExist input
        | otherwise = return False -- indicate the error? throw? output? fail in parter?
