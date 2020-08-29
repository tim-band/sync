{-# LANGUAGE OverloadedStrings #-}

module Transformer
    ( parseValue
    ) where

import Control.Applicative ((<$>))
import Control.Monad (sequence)
import Data.Aeson.Types (parseFail, prependFailure)
import Data.Monoid ((<>))
import Data.Yaml (Object, Array, Value(..), Parser, decodeFileThrow, withArray, withObject, (.:))
import qualified Path
import qualified Copy
import qualified Drives
import qualified Directories
import TransformerParser (PathFinder, PathFinderO, PathFinderV, chain)
import Data.List (foldl')
import qualified Data.Vector as Vec

tags :: [(String, PathFinderO)]
tags =
    [ (Copy.name, Copy.parser)
    , (Path.name, Path.parser)
    , (Drives.name, Drives.parser)
    , (Directories.name, Directories.parser)
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
