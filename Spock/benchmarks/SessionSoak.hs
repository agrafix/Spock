{-# LANGUAGE OverloadedStrings #-}

-- Run each configuration in a separate process so heap measurements are comparable.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception (bracket)
import Control.Monad (forM_, replicateM_, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Test
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.Mem (performMajorGC)
import Text.Read (readMaybe)
import qualified Web.Spock as Spock
import Web.Spock.Config
import qualified Web.Spock.Core as Core
import Web.Spock.Internal.SessionManager (sm_closeSessionManager)
import Web.Spock.Internal.SessionVault (newStmSessionStore')

main :: IO ()
main = do
  supplied <- getArgs
  let args = if null supplied then ["default", "discard", "10000", "3"] else supplied
  case args of
    [mode, cookies, batchText, roundsText]
      | mode `elem` ["default", "core", "always", "on-demand", "disabled"],
        cookies `elem` ["discard", "reuse"],
        Just batch <- readMaybe batchText, batch > 0,
        Just rounds <- readMaybe roundsText, rounds > 0 ->
          bracket (application mode) (\(_, _, close) -> close) $ \(app, sessionCount, _) -> do
            previous <- newIORef =<< getRTSStats
            putStrLn "round,requests,seconds,allocated_bytes,live_bytes,sessions,open_fds"
            forM_ [1 .. rounds :: Int] $ \roundNumber -> do
              start <- getMonotonicTimeNSec
              if cookies == "reuse"
                then Test.runSession (do
                  unless (mode `elem` ["core", "disabled"]) $ check "/touch"
                  replicateM_ batch $ check "/") app
                else replicateM_ batch $ Test.runSession (check "/") app
              end <- getMonotonicTimeNSec
              report previous sessionCount roundNumber batch (fromIntegral (end - start) / 1e9)
              -- TTL and sweep interval are one second. Check after expiry too.
              threadDelay 2200000
              remaining <- sessionCount
              unless (remaining == 0) $ fail "Expired sessions survived housekeeping"
            report previous sessionCount 0 0 0
    _ -> fail "Usage: session-soak (default|core|always|on-demand|disabled) (discard|reuse) REQUESTS ROUNDS +RTS -T"

check :: LBS.ByteString -> Test.Session ()
check path = do
  response <- Test.srequest $ Test.SRequest (Test.setPath Wai.defaultRequest $ LBS.toStrict path) ""
  unless (Test.simpleBody response == "hi") $ fail "Unexpected benchmark response"

application :: String -> IO (Wai.Application, IO Int, IO ())
application mode = do
  store <- newStmSessionStore'
  let count = length <$> ss_runTx store (ss_toList store)
  if mode == "core"
    then do
      app <- Core.spockAsApp $ Core.spockT id $ Core.get Core.root $ Core.text "hi"
      pure (app, count, pure ())
    else do
      cfg <- defaultSpockCfg () PCNoDatabase ()
      close <- newEmptyMVar
      let sessionMode = case mode of
            "always" -> SessionsAlways
            "on-demand" -> SessionsOnDemand
            "disabled" -> SessionsDisabled
            _ -> sc_sessionMode $ spc_sessionCfg cfg
          sessions = (spc_sessionCfg cfg)
            { sc_sessionMode = sessionMode, sc_sessionTTL = 1,
              sc_backend = ServerSessions $ (defaultServerSessionCfg $ SessionStoreInstance store)
                { ssc_housekeepingInterval = 1 } }
      app <- Spock.spockAsApp $ Spock.spock (cfg { spc_sessionCfg = sessions }) $ do
        manager <- lift Spock.getSessMgr
        liftIO $ putMVar close $ sm_closeSessionManager manager
        Spock.get Spock.root $ Spock.text "hi"
        Spock.get "touch" $ Spock.writeSession () >> Spock.text "hi"
      pure (app, count, joinClose close)
  where
    joinClose close = readMVar close >>= id

report :: IORef RTSStats -> IO Int -> Int -> Int -> Double -> IO ()
report previous count roundNumber requests seconds = do
  performMajorGC
  stats <- getRTSStats
  old <- atomicModifyIORef' previous $ \old -> (stats, old)
  sessions <- count
  fds <- openDescriptors
  putStrLn $ comma [show roundNumber, show requests, show seconds,
    show $ allocated_bytes stats - allocated_bytes old,
    show $ gcdetails_live_bytes $ gc stats, show sessions, show fds]
  where
    comma [] = ""
    comma [x] = x
    comma (x:xs) = x ++ "," ++ comma xs

openDescriptors :: IO Int
openDescriptors = go ["/proc/self/fd", "/dev/fd"]
  where
    go [] = pure (-1) -- Not available on every platform.
    go (path:paths) = do
      exists <- doesDirectoryExist path
      if exists then length <$> listDirectory path else go paths
