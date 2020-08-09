{-# LANGUAGE OverloadedStrings #-}

module Transformer
    ( parseValue
    ) where

import Control.Applicative ((<$>), (<|>))
import Control.Monad (sequence)
import Data.Yaml (Object, Array, Value, Parser, decodeFileThrow, withArray, withObject, (.:))
import qualified Path
import qualified Copy
import TransformerParser (PathFinder, PathFinderO, PathFinderV, chain)
import Data.List (foldl')
import qualified Data.Vector as Vec

tags :: [(String, PathFinderO)]
tags =
    [ (Copy.name, Copy.parser)
    , (Path.name, Path.parser)
    ]

parseObject :: PathFinderO
parseObject tr ob = do
    tag <- ob .: "t"
    case (lookup tag tags) of
        Nothing -> fail ("Unknown tag " ++ tag)
        Just f -> f tr ob

parseArray :: PathFinderV -> Array -> Parser PathFinder
parseArray tr ar = let
    pfs :: Parser [PathFinder]
    pfs = sequence $ Vec.toList $ Vec.map tr ar
    runAll :: [PathFinder] -> FilePath -> IO [FilePath]
    runAll fs p = foldl' chain (return [p]) fs
    in runAll <$> pfs

parseValue :: PathFinderV
parseValue v = withArray "array" (parseArray parseValue) v
    <|>  withObject "object" (parseObject parseValue) v
