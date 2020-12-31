module Get (name, parser) where

import Data.Text (pack)
import Data.Yaml ((.:))

import Log (logInfo)
import PathFinder (Output, PathFinderO, getState)

name :: String
name = "get"

parser :: PathFinderO
parser _ ob = get <$> ob .: pack "from" where
    get :: String -> FilePath -> Output
    get from _ = getState from
