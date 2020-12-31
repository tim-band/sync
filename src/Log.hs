module Log (logInfo, logWarning) where

import Control.Monad.Trans.Class (lift)

logInfo s = (lift . putStrLn) ("INFO: " ++ s)
logWarning s = (lift . putStrLn) ("WARNING: " ++ s)
