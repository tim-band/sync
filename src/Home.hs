module Home (name, parser) where

import Control.Applicative (pure)
import Control.Monad (mapM_)
import Control.Monad.Trans.Class (lift)
import System.Directory (getHomeDirectory)

import Log (logInfo)
import PathFinder (PathFinderO)

name = "home"

-- the value doesn't matter because there are no interesting values in it
parser :: PathFinderO
parser _ _ = pure $ \_ -> do
    home <- lift getHomeDirectory
    logInfo $ "home: " ++ home
    return [home]
