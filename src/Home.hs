module Home (name, parser) where

import Control.Applicative (pure)
import Control.Monad (mapM_)
import TransformerParser (PathFinderO)
import System.Directory (getHomeDirectory)
import Log (logInfo)

name = "home"

-- the value doesn't matter because there are no interesting values in it
parser :: PathFinderO
parser _ _ = pure $ \_ -> do
    home <- getHomeDirectory
    logInfo $ "home: " ++ home
    return [home]