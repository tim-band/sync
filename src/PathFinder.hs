module PathFinder
    ( PathFinder
    , PathFinderO
    , PathFinderV(..)
    , chain
    ) where

import Data.Yaml (Value, Object, Parser)
import Control.Applicative (liftA2)
import Control.Monad (mapM)

type PathFinder = FilePath -> IO [FilePath]
type PathFinderV = Value -> Parser PathFinder
type PathFinderO = PathFinderV -> Object -> Parser PathFinder

chain :: IO [FilePath] -> (FilePath -> IO [FilePath]) -> IO [FilePath]
chain iops f = do
    ps <- iops
    pss <- mapM f ps
    return $ concat pss

infixl 1 `chain`
