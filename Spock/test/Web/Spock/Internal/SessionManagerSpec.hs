{-# LANGUAGE OverloadedStrings #-}

module Web.Spock.Internal.SessionManagerSpec (spec) where

import Control.Concurrent.STM
import Data.IORef
import Data.Time
import qualified Data.Vault.Lazy as V
import Test.Hspec
import Web.Spock.Config
import Web.Spock.Internal.SessionManager
import Web.Spock.Internal.SessionVault

spec :: Spec
spec =
  describe "Session Manager" $
    do
      it "should return the correct csrf token" $
        do
          mgr <- mkMgr
          sm_getCsrfToken mgr `shouldReturn` "fake-token"
      it "should not loose data on session id regeneration" $
        do
          mgr <- mkMgr
          sm_writeSession mgr True
          sm_regenerateSessionId mgr
          sm_readSession mgr `shouldReturn` True
      it "should modify session correctly" $
        do
          mgr <- mkMgr
          x <- sm_modifySession mgr (const (True, True))
          x `shouldBe` True
          sm_readSession mgr `shouldReturn` True
      it "should remember session content" $
        do
          mgr <- mkMgr
          sm_writeSession mgr True
          sm_readSession mgr `shouldReturn` True
      it "writing to the session after clearing all should not crash" $
        do
          mgr <- mkMgr
          sm_writeSession mgr True
          sm_clearAllSessions mgr
          sm_writeSession mgr True
      it "should be possible to map over all sessions" $
        do
          mgr <- mkMgr
          sm_writeSession mgr True
          sm_readSession mgr `shouldReturn` True
          sm_mapSessions mgr (const $ return False)
          sm_readSession mgr `shouldReturn` False

mkMgr :: IO (SessionManager IO conn Bool st)
mkMgr =
  do
    sessionCfg <- defaultSessionCfg False
    sv <- newStmSessionStore'
    let testSession =
          Session
            { sess_id = "fake-sid",
              sess_csrfToken = "fake-token",
              sess_validUntil = UTCTime (fromGregorian 2030 1 1) 0,
              sess_data = False
            }
    atomically $
      ss_storeSession sv testSession
    let sessionCfg' = sessionCfg {sc_store = SessionStoreInstance sv}
    k <- V.newKey
    sessionVaultR <- newIORef $ V.insert k (sess_id testSession) V.empty
    mgr <-
      createSessionManager sessionCfg' $
        SessionIf
          { si_queryVault =
              \key ->
                do
                  vault <- readIORef sessionVaultR
                  return $ V.lookup key vault,
            si_modifyVault = modifyIORef sessionVaultR,
            si_setRawMultiHeader = \_ _ -> return (),
            si_vaultKey = return k
          }
    return mgr
