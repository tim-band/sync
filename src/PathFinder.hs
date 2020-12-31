module PathFinder
  ( Output
  , PathFinder
  , PathFinderO
  , PathFinderV(..)
  , chain
  , getState
  , setState
  ) where

import Data.Map (Map, insert, findWithDefault)
import Data.Yaml (Value, Object, Parser)
import Control.Applicative (liftA2)
import Control.Monad (mapM)
import Control.Monad.Trans.State.Strict (StateT, modify', get)

-- type Output = IO [FilePath]
type Output = StateT (Map String [FilePath]) IO [FilePath]

type PathFinder = FilePath -> Output
type PathFinderV = Value -> Parser PathFinder
type PathFinderO = PathFinderV -> Object -> Parser PathFinder

chain :: Output -> PathFinder -> Output
chain iops f = do
  ps <- iops
  pss <- mapM f ps
  return $ concat pss

infixl 1 `chain`

setState :: String -> [FilePath] -> Output
setState name paths = do
  modify' (insert name paths)
  return paths

getState :: String -> Output
getState name = findWithDefault [] name <$> get
