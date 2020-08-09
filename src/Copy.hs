module Copy (name, parser) where

import Control.Monad.Extra (ifM)
import Data.Text (pack)
import Data.Yaml (Value, Parser, (.:))
import TransformerParser (PathFinderO, PathFinder, chain)
import System.Directory (createDirectoryIfMissing, copyFile, doesPathExist)
import System.Posix.Directory (changeWorkingDirectory)

name = "copy"

(..:) ob s = (Data.Yaml..:) ob (pack s)

-- from-root
-- to-root
-- from-file
-- to-file (from from-file, cwd is from-root)
parser :: PathFinderO
parser tr ob = do
    fr <- ob ..: "from-root"
    tr <- ob ..: "to-root"
    ff <- ob ..: "from-file"
    tf <- ob ..: "to-file"
    doCopyV fr tr ff tf where
    doCopyV :: Value -> Value -> Value -> Value -> Parser PathFinder
    doCopyV frv trv ffv tfv = do
        frf <- tr frv
        trf <- tr tfv
        fff <- tr ffv
        tff <- tr tfv
        return $ doCopy frf trf fff tff
    doCopy :: PathFinder
            -> PathFinder
            -> PathFinder
            -> PathFinder
            -> PathFinder
    doCopy fromRootV toRootV fromFileV toFileV _ =
        fromRootV "/" `chain` \fromRoot ->
            toRootV "/" `chain` \toRoot ->
                fromFileV fromRoot `chain` \fromFile -> do
                    changeWorkingDirectory fromRoot
                    toFileV fromFile `chain` \toFile -> do
                        ifM (doesPathExist toFile) (return []) $
                            (copyFile fromFile toFile >> return [toFile])
