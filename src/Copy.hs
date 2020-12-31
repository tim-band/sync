module Copy (name, parser) where

import Control.Monad.Extra (ifM)
import Control.Monad.Trans.Class (lift)
import Data.Text (pack)
import Data.Yaml (Value, Parser, (.:), (.:?))
import System.Directory (createDirectoryIfMissing, copyFile, doesPathExist, makeRelativeToCurrentDirectory)
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory, (</>))
import System.Posix.Directory (changeWorkingDirectory)
import System.Process (spawnCommand, waitForProcess)

import Log (logInfo, logWarning)
import PathFinder (PathFinderO, PathFinder, chain)

name = "copy"

(..:) ob s = (Data.Yaml..:) ob (pack s)
(..:?) ob s = (Data.Yaml..:?) ob (pack s)

skip :: IO ()
skip = return ()

strReplace :: String -> String -> String -> String
strReplace needle replacement haystack = sr needle haystack where
  sr [] hs = replacement ++ strReplace needle replacement hs
  sr (_:_) [] = haystack
  sr (n:ns) (h:hs)
    | n == h = sr ns hs
    | otherwise = case haystack of
      [] -> []
      (h:hs) -> h : strReplace needle replacement hs

-- from-root
-- to-root
-- from-file
-- to-file (from from-file, cwd is from-root)
-- cmd shell command to run instead of copying (%1 becomes fromFile, %2 becomes toPath)
parser :: PathFinderO
parser trans ob = do
    fr <- ob ..: "from-root"
    tr <- ob ..: "to-root"
    ff <- ob ..: "from-file"
    tf <- ob ..: "to-file"
    cmd <- ob  ..:? "cmd"
    doCopyV fr tr ff tf cmd where
    doCopyV :: Value -> Value -> Value -> Value -> Maybe String -> Parser PathFinder
    doCopyV frv trv ffv tfv cmd = do
        frf <- trans frv
        trf <- trans trv
        fff <- trans ffv
        tff <- trans tfv
        return $ doCopy frf trf fff tff cmd
    doCopy :: PathFinder
            -> PathFinder
            -> PathFinder
            -> PathFinder
            -> Maybe String
            -> PathFinder
    doCopy fromRootV toRootV fromFileV toFileV cmd _ =
        fromRootV "/" `chain` \fromRoot ->
            toRootV "/" `chain` \toRoot ->
                fromFileV fromRoot `chain` \fromFile -> do
                    logInfo $ "fromRoot: " ++ fromRoot ++ ", toRoot: " ++ toRoot ++ ", fromFile: " ++ fromFile
                    lift $ changeWorkingDirectory fromRoot
                    fromFileRelative <- lift $ makeRelativeToCurrentDirectory fromFile
                    toFileV fromFileRelative `chain` \toFile -> do
                        logInfo $ "maybe copy " ++ fromFile ++ " to " ++ toFile
                        let toPath = toRoot </> toFile
                        ifM (lift $ doesPathExist toPath) (logInfo ("destination already exists: " ++ toPath) >> return []) $ do
                            logInfo $ "to " ++ toPath
                            let toPathDir = takeDirectory toPath
                            lift $ createDirectoryIfMissing True toPathDir
                            case cmd of
                              Nothing -> lift (copyFile fromFile toPath) >> logInfo ("Copying " ++ fromFile ++ " to " ++ toPath)
                              Just cmdt -> do
                                let esc s = '\'' : strReplace "'" "'\\''" s ++ "'"
                                let cmdr = strReplace "%1" (esc fromFile) $ strReplace "%2" (esc toPath) cmdt
                                h <- lift $ spawnCommand cmdr
                                e <- lift $ waitForProcess h
                                case e of
                                  ExitSuccess -> lift skip
                                  ExitFailure n -> logWarning $ "Error code " ++ show n ++ " from " ++ cmdr
                            return [toPath]
