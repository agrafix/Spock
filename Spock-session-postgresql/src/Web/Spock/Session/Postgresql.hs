{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | An optional persistent session store. Initialize the schema once before
-- starting workers, then share a namespace across workers using the same
-- session data type. The caller owns the connection pool.
module Web.Spock.Session.Postgresql
  ( PostgresqlTx,
    PostgresqlSessionCfg (..),
    defaultPostgresqlSessionCfg,
    SessionDecodeError (..),
    initializePostgresqlSessions,
    newPostgresqlSessionStore,
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, catch, throwIO)
import Control.Monad (forM_, unless, void)
import Control.Monad.Reader (ReaderT (..))
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Transaction
import Web.Spock.Config
import Web.Spock.Internal.SessionManager (Session (..))

-- | Deliberately has no MonadIO instance: the whole transaction can be retried,
-- so callbacks must not perform external side effects.
newtype PostgresqlTx a = PostgresqlTx (ReaderT Connection IO a)
  deriving (Functor, Applicative, Monad)

data PostgresqlSessionCfg = PostgresqlSessionCfg
  { -- | Independent applications must use different namespaces.
    psc_namespace :: Text,
    -- | Maximum retries for serialization failures or deadlocks. Other errors
    -- propagate immediately. Exhaustion propagates the last SQL exception.
    psc_maxRetries :: Int
  }

defaultPostgresqlSessionCfg :: PostgresqlSessionCfg
defaultPostgresqlSessionCfg = PostgresqlSessionCfg "spock" 20

-- | Stored JSON cannot be decoded as the application's session type. No stored
-- content is included in this exception.
data SessionDecodeError = SessionDecodeError deriving (Eq, Show)
instance Exception SessionDecodeError

-- | Create the adapter's table. Run once during deployment/startup, before
-- concurrent workers. All ordinary operations are scoped to a namespace.
initializePostgresqlSessions :: Connection -> IO ()
initializePostgresqlSessions connection = void $ execute_ connection
  "CREATE TABLE IF NOT EXISTS spock_sessions (\
  \namespace TEXT NOT NULL, session_id TEXT NOT NULL, csrf_token TEXT NOT NULL,\
  \valid_until TIMESTAMPTZ NOT NULL, session_data JSONB NOT NULL,\
  \PRIMARY KEY (namespace, session_id))"

newPostgresqlSessionStore :: (ToJSON sess, FromJSON sess) =>
  PostgresqlSessionCfg -> Pool Connection -> IO (SessionStore (Session conn sess st) PostgresqlTx)
newPostgresqlSessionStore cfg pool = do
  unless (psc_maxRetries cfg >= 0) $ ioError $ userError "psc_maxRetries must be nonnegative"
  let namespace = psc_namespace cfg
      store = SessionStore
        { ss_runTx = \(PostgresqlTx action) -> withResource pool $ \connection ->
            retryTransaction (psc_maxRetries cfg) connection (runReaderT action connection),
          ss_loadSession = \sid -> onConnection $ \connection -> do
            rows <- query connection
              "SELECT session_id, csrf_token, valid_until, session_data::text FROM spock_sessions WHERE namespace = ? AND session_id = ?"
              (namespace, sid)
            case rows of
              [] -> pure Nothing
              row : _ -> Just <$> decodeSession row,
          ss_deleteSession = \sid -> onConnection $ \connection -> void $ execute connection
            "DELETE FROM spock_sessions WHERE namespace = ? AND session_id = ?" (namespace, sid),
          ss_storeSession = \session -> onConnection $ \connection -> void $ execute connection
            "INSERT INTO spock_sessions (namespace, session_id, csrf_token, valid_until, session_data) VALUES (?, ?, ?, ?, ?::jsonb) \
            \ON CONFLICT (namespace, session_id) DO UPDATE SET csrf_token = EXCLUDED.csrf_token, valid_until = EXCLUDED.valid_until, session_data = EXCLUDED.session_data"
            (namespace, sess_id session, sess_csrfToken session, sess_validUntil session, T.decodeUtf8 $ LBS.toStrict $ encode $ sess_data session),
          ss_toList = onConnection $ \connection -> do
            rows <- query connection
              "SELECT session_id, csrf_token, valid_until, session_data::text FROM spock_sessions WHERE namespace = ? ORDER BY session_id" (Only namespace)
            mapM decodeSession rows,
          ss_filterSessions = \predicate -> do
            sessions <- ss_toList store
            forM_ sessions $ \session -> unless (predicate session) $ ss_deleteSession store (sess_id session),
          ss_mapSessions = \f -> do
            sessions <- ss_toList store
            forM_ sessions $ \session -> do
              updated <- f session
              -- Like the STM store, mapping preserves the entry's key.
              ss_storeSession store (updated { sess_id = sess_id session })
        }
  pure store

onConnection :: (Connection -> IO a) -> PostgresqlTx a
onConnection = PostgresqlTx . ReaderT

decodeSession :: FromJSON sess => (Text, Text, UTCTime, Text) -> IO (Session conn sess st)
decodeSession (sid, csrf, expiry, payload) =
  case eitherDecodeStrict' (T.encodeUtf8 payload) of
    Left _ -> throwIO SessionDecodeError
    Right value -> pure $ Session sid csrf expiry value

retryTransaction :: Int -> Connection -> IO a -> IO a
retryTransaction limit connection action = go 0
  where
    go attempt = withTransactionMode (TransactionMode Serializable ReadWrite) connection action
      `catch` \(err :: SqlError) ->
        if sqlState err `elem` ["40001", "40P01"] && attempt < limit
          then do
            threadDelay (min 100000 (1000 * (attempt + 1)))
            go (attempt + 1)
          else throwIO err
