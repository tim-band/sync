module Copy (name, parser) where

import Control.Monad.Extra (ifM)
import Data.Text (pack)
import Data.Yaml (Value, Parser, (.:))
import System.Directory (createDirectoryIfMissing, copyFile, doesPathExist)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Directory (changeWorkingDirectory)

import Log (logInfo)
import PathFinder (PathFinderO, PathFinder, chain)

name = "copy"

(..:) ob s = (Data.Yaml..:) ob (pack s)

-- from-root
-- to-root
-- from-file
-- to-file (from from-file, cwd is from-root)
parser :: PathFinderO
parser trans ob = do
    fr <- ob ..: "from-root"
    tr <- ob ..: "to-root"
    ff <- ob ..: "from-file"
    tf <- ob ..: "to-file"
    doCopyV fr tr ff tf where
    doCopyV :: Value -> Value -> Value -> Value -> Parser PathFinder
    doCopyV frv trv ffv tfv = do
        frf <- trans frv
        trf <- trans trv
        fff <- trans ffv
        tff <- trans tfv
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
                    logInfo $ "fromRoot: " ++ fromRoot ++ ", toRoot: " ++ toRoot ++ ", fromFile: " ++ fromFile
                    changeWorkingDirectory fromRoot
                    toFileV fromFile `chain` \toFile -> do
                        logInfo $ "maybe copy " ++ fromFile
                        let toPath = toRoot </> toFile
                        ifM (doesPathExist toPath) (return []) $ do
                            logInfo $ "to " ++ toPath
                            let toPathDir = takeDirectory toPath
                            createDirectoryIfMissing True toPathDir
                            copyFile fromFile toPath
                            return [toPath]
