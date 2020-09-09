module Filter (name, parser) where

import qualified Data.Yaml
import Data.List.Extra (split)
import Data.Maybe (fromMaybe)
import Data.Text (pack, unpack)
import qualified Data.Vector as Vec
import System.Directory (createDirectoryIfMissing, copyFile, doesPathExist, doesDirectoryExist, doesFileExist)
import System.FilePath (takeFileName)
import System.Posix.Directory (changeWorkingDirectory)

import Log (logInfo)
import PathFinder (PathFinderO, chain)

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
matches [] []  = True

parser :: PathFinderO
parser _ ob = doFilter
        <$> ob .:? "pattern"
        <*> ob .:? "whole-path" .!= False
        <*> ob .:? "type" .!= "?" where
    doFilter :: Maybe Data.Yaml.Value -> Bool -> String -> FilePath -> IO [FilePath]
    doFilter patterns wholePath ftype input = case matchesPatterns patterns wholePath input of
        True -> do
            typeOk <- matchesType ftype input
            if typeOk
                then do
                    logInfo $ "Filter kept: " ++ input
                    return [input]
                else do
                    logInfo $ "Wrong type (not " ++ ftype ++ "): " ++ input
                    return []
        False -> do
            logInfo $ "Pattern (" ++ show patterns ++ ") match failed: " ++ input
            return []
    matchesPatterns :: Maybe Data.Yaml.Value -> Bool -> FilePath -> Bool
    matchesPatterns Nothing _ _ = True
    matchesPatterns (Just v) wholePath input = case v of
        Data.Yaml.String s -> matchesPattern (unpack s) wholePath input
        Data.Yaml.Array ar -> Vec.any matchesValue ar where
            matchesValue v = case v of
                Data.Yaml.String s -> matchesPattern (unpack s) wholePath input
                _ -> error "Fix Filter-- array of non-strings pattern should not have parsed"
        _ -> error "Fix Filter-- non-string non-array pattern should not have parsed"
    matchesPattern :: String -> Bool -> FilePath -> Bool
    matchesPattern pattern wholePath input = let
        patternBits = split (== '*') pattern
        fn = if wholePath then input else takeFileName input in
            matches pattern fn
    matchesType :: String -> FilePath -> IO Bool
    matchesType t input
        | t == "?" = return True
        | t == "f" = doesFileExist input
        | t == "d" = doesDirectoryExist input
        | otherwise = return False -- indicate the error? throw? output? fail in parter?
