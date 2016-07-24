{-# LANGUAGE OverloadedStrings #-}
module Web.Spock.Internal.SessionManagerSpec (spec) where

import Web.Spock.Config
import Web.Spock.Internal.SessionManager
import Web.Spock.Internal.SessionVault

import Control.Concurrent.STM
import Data.IORef
import Data.Time
import Test.Hspec
import qualified Data.Vault.Lazy as V

spec :: Spec
spec =
    describe "Session Manager" $
    do it "clear all sessions should not crash" $
           do mgr <- mkMgr
              sm_writeSession mgr True
              sm_clearAllSessions mgr

mkMgr :: IO (SessionManager IO conn Bool st)
mkMgr =
    do sessionCfg <- defaultSessionCfg False
       sv <- newStmSessionStore'
       let testSession =
               Session
               { sess_id = "fake-sid"
               , sess_csrfToken = "fake-token"
               , sess_validUntil = UTCTime (fromGregorian 2030 1 1) 0
               , sess_data = False
               }
       atomically $
           ss_storeSession sv testSession
       let sessionCfg' = sessionCfg { sc_store = SessionStoreInstance sv }
       k <- V.newKey
       sessionVaultR <- newIORef $ V.insert k (sess_id testSession) V.empty
       mgr <-
           createSessionManager sessionCfg' $
           SessionIf
           { si_queryVault =
                   \key ->
                   V.lookup key <$> readIORef sessionVaultR
           , si_modifyVault = modifyIORef sessionVaultR
           , si_setRawMultiHeader = \_ _ -> return ()
           , si_vaultKey = return k
           }
       return mgr
