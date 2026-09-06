{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async (concurrently_, mapConcurrently)
import Control.Exception (bracket)
import Control.Monad (forM_, replicateM, replicateM_, void)
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Data.List (sort)
import Data.Pool
import qualified Data.Text as T
import Data.Time
import qualified Data.Vault.Lazy as V
import Database.PostgreSQL.Simple
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Test.Hspec
import Web.Spock.Config
import Web.Spock.Internal.SessionManager
import Web.Spock.Session.Postgresql

type Store = SessionStore (Session () Int ()) PostgresqlTx
data Fixture = Fixture
  { firstStore :: Store, secondStore :: Store, otherStore :: Store,
    firstPool :: Pool Connection, secondPool :: Pool Connection, namespace :: T.Text }

main :: IO ()
main = do
  connectionString <- maybe (fail "Set SPOCK_TEST_POSTGRESQL to a disposable test database connection string")
    (pure . BS.pack) =<< lookupEnv "SPOCK_TEST_POSTGRESQL"
  bracket (connectPostgreSQL connectionString) close initializePostgresqlSessions
  hspec $ around (withFixture connectionString) $ describe "PostgreSQL sessions (real database)" $ do
    it "persists JSON, CSRF tokens, and expiry across pool restarts" $ \fixture -> do
      let first = firstStore fixture
          second = secondStore fixture
      ss_runTx first $ ss_storeSession first (session "saved" 42)
      destroyAllResources $ firstPool fixture
      loaded <- ss_runTx second $ ss_loadSession second "saved"
      fmap (\s -> (sess_id s, sess_csrfToken s, sess_validUntil s, sess_data s)) loaded
        `shouldBe` Just ("saved", "csrf-token", expiry, 42)

    it "preserves every concurrent read/modify/write across independent pools" $ \fixture -> withinTimeout $ do
      let first = firstStore fixture
          second = secondStore fixture
      ss_runTx first $ ss_storeSession first (session "counter" 0)
      results <- mapConcurrently (\store -> replicateM 20 $ increment store) (take 8 $ cycle [first, second])
      sort (concat results) `shouldBe` [1 .. 160]
      fmap sess_data <$> ss_runTx second (ss_loadSession second "counter") `shouldReturn` Just 160

    it "never restores a deleted session through concurrent renewal" $ \fixture -> withinTimeout $ do
      let first = firstStore fixture
          second = secondStore fixture
      ss_runTx first $ ss_storeSession first (session "counter" 0)
      concurrently_
        (replicateM_ 40 $ ss_runTx first $ do
          found <- ss_loadSession first "counter"
          forM_ found $ \s -> ss_storeSession first (s { sess_validUntil = addUTCTime 1 $ sess_validUntil s }))
        (ss_runTx second $ ss_deleteSession second "counter")
      fmap sess_id <$> ss_runTx first (ss_loadSession first "counter") `shouldReturn` Nothing

    it "scopes load, list, map, and deletion to the selected namespace" $ \fixture -> do
      let store = firstStore fixture
          other = otherStore fixture
      ss_runTx store $ forM_ [session "a" 1, session "b" 2] (ss_storeSession store)
      ss_runTx other $ ss_storeSession other (session "a" 99)
      ss_runTx store $ ss_mapSessions store (\s -> pure s { sess_data = sess_data s + 10 })
      map sess_data <$> ss_runTx store (ss_toList store) `shouldReturn` [11, 12]
      ss_runTx store $ ss_filterSessions store ((> 11) . sess_data)
      map sess_id <$> ss_runTx store (ss_toList store) `shouldReturn` ["b"]
      ss_runTx store $ ss_filterSessions store (const False)
      length <$> ss_runTx store (ss_toList store) `shouldReturn` 0
      fmap sess_data <$> ss_runTx other (ss_loadSession other "a") `shouldReturn` Just 99

    it "rolls back the entire transaction on a decoding failure and remains usable" $ \fixture -> do
      let store = firstStore fixture
      withResource (firstPool fixture) $ \connection -> void $ execute connection
        "INSERT INTO spock_sessions VALUES (?, 'bad', 'token', ?, '\"not-an-int\"'::jsonb)"
        (namespace fixture, expiry)
      ss_runTx store (do
        ss_storeSession store (session "rollback" 7)
        ss_loadSession store "bad") `shouldThrow` (== SessionDecodeError)
      fmap sess_id <$> ss_runTx store (ss_loadSession store "rollback") `shouldReturn` Nothing
      ss_runTx store $ ss_storeSession store (session "after-error" 8)
      fmap sess_data <$> ss_runTx store (ss_loadSession store "after-error") `shouldReturn` Just 8

    it "treats hostile session IDs as parameter values" $ \fixture -> do
      let store = firstStore fixture
          key = "'; DELETE FROM spock_sessions; --"
      ss_runTx store $ ss_storeSession store (session key 7)
      fmap sess_data <$> ss_runTx store (ss_loadSession store key) `shouldReturn` Just 7
      ss_runTx store $ ss_deleteSession store key
      length <$> ss_runTx store (ss_toList store) `shouldReturn` 0

    it "honors expiration and logout through the actual session manager" $ \fixture -> do
      let store = firstStore fixture
      ss_runTx store $ ss_storeSession store ((session "expired" 42) { sess_validUntil = UTCTime (fromGregorian 1970 1 1) 0 })
      cfg <- defaultSessionCfg (0 :: Int)
      key <- V.newKey
      vault <- newIORef $ V.insert key "expired" V.empty
      let sessionIf = SessionIf
            { si_queryVault = \k -> V.lookup k <$> readIORef vault,
              si_modifyVault = modifyIORef' vault,
              si_setRawMultiHeader = \_ _ -> pure (),
              si_vaultKey = pure key }
      withSessionManager (cfg { sc_backend = ServerSessions $ defaultServerSessionCfg $ SessionStoreInstance store }) sessionIf $ \manager -> do
        sm_readSession manager `shouldReturn` 0
        sm_writeSession manager 5
        sm_regenerateSessionId manager
        sm_readSession manager `shouldReturn` 5
        sm_destroySession manager
        length <$> ss_runTx store (ss_toList store) `shouldReturn` 0

    it "rejects negative retry limits" $ \fixture ->
      (newPostgresqlSessionStore (defaultPostgresqlSessionCfg { psc_maxRetries = -1 }) (firstPool fixture) :: IO Store)
        `shouldThrow` anyIOException

expiry :: UTCTime
expiry = UTCTime (fromGregorian 2030 1 1) 0

session :: T.Text -> Int -> Session () Int ()
session sid = Session sid "csrf-token" expiry

increment :: Store -> IO Int
increment store = ss_runTx store $ do
  current <- ss_loadSession store "counter"
  case current of
    Nothing -> pure (-1)
    Just value -> do
      let next = sess_data value + 1
      ss_storeSession store (value { sess_data = next })
      pure next

withinTimeout :: IO () -> Expectation
withinTimeout action = timeout 30000000 action `shouldReturn` Just ()

withFixture :: BS.ByteString -> (Fixture -> IO ()) -> IO ()
withFixture connectionString run = bracket create cleanup run
  where
    pool = newPool $ setNumStripes (Just 1) $ defaultPoolConfig (connectPostgreSQL connectionString) close 60 8
    create = do
      first <- pool
      second <- pool
      name <- ("spock-tests-" <>) . T.pack . show <$> getCurrentTime
      let config = defaultPostgresqlSessionCfg { psc_namespace = name, psc_maxRetries = 100 }
      store1 <- newPostgresqlSessionStore config first
      store2 <- newPostgresqlSessionStore config second
      other <- newPostgresqlSessionStore (config { psc_namespace = name <> "-other" }) second
      pure $ Fixture store1 store2 other first second name
    cleanup fixture = do
      withResource (secondPool fixture) $ \connection -> void $ execute connection
        "DELETE FROM spock_sessions WHERE namespace IN (?, ?)" (namespace fixture, namespace fixture <> "-other")
      destroyAllResources $ firstPool fixture
      destroyAllResources $ secondPool fixture
