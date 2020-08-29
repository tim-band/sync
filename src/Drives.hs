module Drives (name, parser) where

import Control.Applicative (pure)
import Control.Monad (mapM_)
import TransformerParser (PathFinderO)
import System.MountPoints (Mntent, mnt_dir, mnt_type, getMounts)
import Log (logInfo)

name = "drives"

logI x = logInfo ("drives: " ++ x)

-- the value doesn't matter because there are no interesting values in it
parser :: PathFinderO
parser _ _ = pure $ \_ -> do
    mnts <- getMounts
    let ds = map mnt_dir mnts
    mapM_ logI ds
    return ds
