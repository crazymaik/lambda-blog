module Lambda.Watchdog
  ( startWatchdog
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (when)
import Network
import System.IO (hClose)

import qualified Lambda.Util.Log as L

startWatchdog :: [String] -> Int -> IO () -> IO ()
startWatchdog hosts port action = do
  s <- listenOn $ PortNumber (fromInteger $ read $ show port)
  forkIO (watchDog hosts s action)
  return ()

watchDog :: [String] -> Socket -> IO () -> IO ()
watchDog hosts s action = do
  (handle, hostname, port) <- accept s
  L.info ("WatchDog connection from " ++ hostname)
  when (isLocal hostname) (log >> action)
  hClose handle
  watchDog hosts s action
  where
    isLocal host = any (==host) hosts
    log = L.info "reloading content"

