{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Web.Spock.Internal.SessionLifecycleSpec (spec) where

import Control.Concurrent
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.STM
import Control.Monad (forM_, void, when)
import Data.IORef
import Data.List (sort)
import Data.Maybe (isNothing)
import Data.Time
import qualified Data.Vault.Lazy as V
import System.Timeout (timeout)
import Test.Hspec
import Web.Spock.Config
import Web.Spock.Internal.SessionManager
import Web.Spock.Internal.SessionVault (newStmSessionStore')

data Fixture = Fixture
  { manager :: SessionManager IO () Int (),
    store :: SessionStore (Session () Int ()) STM,
    pauseCommits :: Int -> IO (),
    awaitCommit :: IO (),
    releaseCommits :: IO ()
  }

spec :: Spec
spec =
  describe "Session lifecycle" $
    forM_ [False, True] $ \expand ->
      describe ("TTL expansion " ++ show expand) $
        around (withFixture expand) $
          do
            it "replaces expired sessions without returning their data" $ \fixture ->
              do
                seedSession fixture (-60) 42
                sm_readSession (manager fixture) `shouldReturn` 0
                sm_getSessionId (manager fixture) `shouldNotReturn` "fake-sid"
                oldSession <- atomically $ ss_loadSession (store fixture) "fake-sid"
                oldSession `shouldSatisfy` isNothing
            it "renews only live sessions when expansion is enabled" $ \fixture ->
              do
                seedSession fixture 60 42
                beforeSession <- atomically $ ss_loadSession (store fixture) "fake-sid"
                sm_readSession (manager fixture) `shouldReturn` 42
                afterSession <- atomically $ ss_loadSession (store fixture) "fake-sid"
                case (beforeSession, afterSession) of
                  (Just old, Just new) ->
                    if expand
                      then sess_validUntil new `shouldSatisfy` (> sess_validUntil old)
                      else sess_validUntil new `shouldBe` sess_validUntil old
                  _ -> expectationFailure "The live session disappeared"
            it "modifies an empty replacement instead of expired data" $ \fixture ->
              do
                seedSession fixture (-60) 42
                sm_modifySession (manager fixture) increment `shouldReturn` 1
                sm_readSession (manager fixture) `shouldReturn` 1
                sm_getSessionId (manager fixture) `shouldNotReturn` "fake-sid"
            it "keeps both concurrent updates and their distinct return values" $ \fixture ->
              withinTimeout $
                do
                  pauseCommits fixture 2
                  withAsync (sm_modifySession (manager fixture) increment) $ \first ->
                    withAsync (sm_modifySession (manager fixture) increment) $ \second ->
                      do
                        awaitCommit fixture
                        awaitCommit fixture
                        releaseCommits fixture
                        results <- sequence [wait first, wait second]
                        sort results `shouldBe` [1, 2]
                  sm_readSession (manager fixture) `shouldReturn` 2
            it "does not let a concurrent read overwrite a write" $ \fixture ->
              withinTimeout $
                do
                  pauseCommits fixture 1
                  withAsync (sm_readSession (manager fixture)) $ \reader ->
                    do
                      awaitCommit fixture
                      sm_writeSession (manager fixture) 42
                      releaseCommits fixture
                      void $ wait reader
                  sm_readSession (manager fixture) `shouldReturn` 42
            it "does not let a concurrent read restore a revoked session" $ \fixture ->
              withinTimeout $
                do
                  pauseCommits fixture 1
                  withAsync (sm_readSession (manager fixture)) $ \reader ->
                    do
                      awaitCommit fixture
                      sm_clearAllSessions (manager fixture)
                      releaseCommits fixture
                      void $ wait reader
                  oldSession <- atomically $ ss_loadSession (store fixture) "fake-sid"
                  oldSession `shouldSatisfy` isNothing
            it "does not let an in-flight modification restore a revoked session" $ \fixture ->
              withinTimeout $
                do
                  pauseCommits fixture 1
                  withAsync (sm_modifySession (manager fixture) increment) $ \writer ->
                    do
                      awaitCommit fixture
                      sm_clearAllSessions (manager fixture)
                      releaseCommits fixture
                      void $ wait writer
                  oldSession <- atomically $ ss_loadSession (store fixture) "fake-sid"
                  oldSession `shouldSatisfy` isNothing
            it "can modify a fresh session after all sessions are cleared" $ \fixture ->
              do
                seedSession fixture 60 42
                sm_clearAllSessions (manager fixture)
                sm_modifySession (manager fixture) increment `shouldReturn` 1
                sm_readSession (manager fixture) `shouldReturn` 1
            it "cannot recreate a session after regeneration commits and sessions are revoked" $ \fixture ->
              withinTimeout $ do
                pauseCommits fixture 1
                withAsync (sm_regenerateSessionId (manager fixture)) $ \regenerator -> do
                  awaitCommit fixture
                  sm_clearAllSessions (manager fixture)
                  releaseCommits fixture
                  wait regenerator
                length <$> atomically (ss_toList $ store fixture) `shouldReturn` 0
            it "does not let an in-flight modification undo logout" $ \fixture ->
              withinTimeout $ do
                pauseCommits fixture 1
                withAsync (sm_modifySession (manager fixture) increment) $ \writer -> do
                  awaitCommit fixture
                  sm_destroySession (manager fixture)
                  releaseCommits fixture
                  void $ wait writer
                length <$> atomically (ss_toList $ store fixture) `shouldReturn` 0
            it "does not preserve expired data when regenerating" $ \fixture -> do
              seedSession fixture (-60) 42
              sm_regenerateSessionId (manager fixture)
              sm_readSession (manager fixture) `shouldReturn` 0
              sm_getCsrfToken (manager fixture) `shouldNotReturn` "fake-token"
  where
    increment value = (value + 1, value + 1)

withinTimeout :: IO () -> Expectation
withinTimeout action = timeout 5000000 action `shouldReturn` Just ()

seedSession :: Fixture -> NominalDiffTime -> Int -> IO ()
seedSession fixture ttl value =
  do
    now <- getCurrentTime
    atomically $
      ss_storeSession (store fixture) (Session "fake-sid" "fake-token" (addUTCTime ttl now) value)

withFixture :: Bool -> (Fixture -> IO ()) -> IO ()
withFixture expand run =
  do
    cfg <- defaultSessionCfg (0 :: Int)
    sessionStore <- newStmSessionStore'
    remaining <- newIORef (0 :: Int)
    committed <- newChan
    resume <- newEmptyMVar
    initialSweep <- newEmptyMVar
    key <- V.newKey
    vault <- newIORef $ V.insert key "fake-sid" V.empty
    let gatedStore =
          sessionStore
            { -- Pause after committing a transaction, never inside STM. This forces
              -- competing operations to expose any gaps between load and store.
              ss_runTx = \action ->
                do
                  result <- atomically action
                  pause <- atomicModifyIORef' remaining $ \n -> (max 0 (n - 1), n > 0)
                  when pause $ writeChan committed () >> readMVar resume
                  pure result
            }
        cfg' =
          cfg
            { sc_store = SessionStoreInstance gatedStore,
              sc_sessionExpandTTL = expand,
              sc_housekeepingInterval = 3600,
              sc_hooks = SessionHooks (\_ -> void $ tryPutMVar initialSweep ())
            }
        sessionIf =
          SessionIf
            { si_queryVault = \k -> V.lookup k <$> readIORef vault,
              si_modifyVault = \f -> atomicModifyIORef' vault (\v -> (f v, ())),
              si_setRawMultiHeader = \_ _ -> pure (),
              si_vaultKey = pure key
            }
    withSessionManager cfg' sessionIf $ \mgr ->
      do
        takeMVar initialSweep
        let fixture = Fixture mgr sessionStore (writeIORef remaining) (readChan committed) (putMVar resume ())
        seedSession fixture 3600 0
        run fixture
