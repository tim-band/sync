{-# LANGUAGE OverloadedStrings #-}

module Transformer
    ( parseValue
    ) where

import Control.Applicative ((<$>))
import Control.Monad (sequence)
import Data.Aeson.Types (parseFail, prependFailure)
import Data.List (foldl')
import Data.Monoid ((<>))
import qualified Data.Vector as Vec
import Data.Yaml (Object, Array, Value(..), Parser, decodeFileThrow, withArray, withObject, (.:))

import qualified All
import qualified Copy
import qualified DatePath
import qualified Directories
import qualified Drives
import qualified Filter
import qualified First
import qualified Home
import qualified Path
import PathFinder (PathFinder, PathFinderO, PathFinderV, chain)

tags :: [(String, PathFinderO)]
tags =
    [ (All.name, All.parser)
    , (Copy.name, Copy.parser)
    , (DatePath.name, DatePath.parser)
    , (Directories.name, Directories.parser)
    , (Drives.name, Drives.parser)
    , (Filter.name, Filter.parser)
    , (First.name, First.parser)
    , (Home.name, Home.parser)
    , (Path.name, Path.parser)
    ]

parseObject :: PathFinderO
parseObject tr ob = do
    tag <- ob .: "t"
    case (lookup tag tags) of
        Nothing -> parseFail ("Unknown tag " ++ tag)
        Just f -> prependFailure ("(" ++ tag ++ ")") $ f tr ob

parseArray :: PathFinderV -> Array -> Parser PathFinder
parseArray tr ar = let
    pfs :: Parser [PathFinder]
    pfs = sequence
        $ zipWith (\i a -> prependFailure ("[" ++ show i ++ "]") (tr a)) [1..]
        $ Vec.toList ar
    runAll :: [PathFinder] -> FilePath -> IO [FilePath]
    runAll fs p = foldl' chain (return [p]) fs
    in runAll <$> pfs

parseValue :: PathFinderV
parseValue v = case v of
    Array ar -> parseArray parseValue ar
    Object ob -> parseObject parseValue ob
    _ -> parseFail ("Expected array or object")
