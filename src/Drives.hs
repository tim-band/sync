module Drives (name, parser) where

import Control.Applicative (pure)
import TransformerParser (PathFinderO)
import System.MountPoints (Mntent, mnt_dir, mnt_type, getMounts)

name = "drives"

-- the value doesn't matter because there are no interesting values in it
parser :: PathFinderO
parser _ _ = pure $ \_ -> do
    mnts <- getMounts
    return $ map mnt_dir mnts
