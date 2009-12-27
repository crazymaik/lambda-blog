module Lambda.Util.Log (
  setupLogging,
  setLogLevel,
  debug,
  info,
  notice,
  warning,
  error,
  critical,
  alert,
  emergency,
  isDebugEnabled
) where

import Control.Monad (when)
import Prelude hiding (error)
import System.Directory (doesFileExist, removeFile)
import qualified System.Log.Logger as L
import qualified System.Log.Handler.Simple as S

root = "Lambda"

setupLogging :: String -> FilePath -> IO ()
setupLogging level logFile = do
  exists <- doesFileExist logFile
  when exists $ removeFile logFile
  h <- S.fileHandler logFile L.DEBUG
  L.updateGlobalLogger L.rootLoggerName (L.setLevel (read level) . L.setHandlers [h])
  notice $ "Configured logger priority to " ++ level

setLogLevel :: String -> IO ()
setLogLevel level = return ()
-- this does not work, don't know why
--  do L.updateGlobalLogger L.rootLoggerName (L.setLevel (read level))
--     notice $ "Configured logger priority to " ++ level

debug = L.debugM root
info = L.infoM root
notice = L.noticeM root
warning = L.warningM root
error = L.errorM root
critical = L.criticalM root
alert = L.alertM root
emergency = L.emergencyM root

isDebugEnabled :: IO Bool
isDebugEnabled = do
  root <- L.getRootLogger
  return (L.getLevel root == L.DEBUG)

