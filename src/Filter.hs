module Filter (name, parser) where

import qualified Data.Yaml
import Data.List.Extra (split)
import Data.Text (pack)
import TransformerParser (PathFinderO, chain)
import System.Directory (createDirectoryIfMissing, copyFile, doesPathExist)
import System.FilePath (takeFileName)
import System.Posix.Directory (changeWorkingDirectory)

name = "filter"

(.:) ob s = (Data.Yaml..:) ob (pack s)

-- match "*" ++ pattern against candidate
matchesStar :: String -> String -> Bool
matchesStar pattern candidate@(_:candidate') = ms pattern candidate where
    ms (p:ps) cs@(c:cs')
        | p == '*' = matchesStar ps cs
        | p == c = ms ps cs'
        | otherwise = matchesStar pattern candidate'
    ms ps@(_:_) [] = matches ps []
    ms [] (_:_) = ms pattern (drop (length candidate - length pattern) candidate)

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
        <$> ob .: "pattern" where
    doFilter :: String -> FilePath -> IO [FilePath]
    doFilter pattern input = let
        patternBits = split (== '*') pattern
        fn = takeFileName input
        value = if matches pattern fn then [input] else [] in
            return value
