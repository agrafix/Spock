{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Spock.Internal.Types where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import qualified Data.HashMap.Strict as HM
import Data.Pool
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime (..))
import Data.Word
import Network.HTTP.Types.Status
import Network.Wai
import Web.Spock.Core

-- | Inside the SpockAllM monad, you may define routes and middleware.
type SpockAllM conn sess st a = SpockT (WebStateM conn sess st) a

-- | The 'SpockActionCtx' is the monad of all route-actions. You have access
-- to the context of the request and database, session and state of your application.
type SpockActionCtx ctx conn sess st = ActionCtxT ctx (WebStateM conn sess st)

-- | The 'SpockAction' is a specialisation of 'SpockActionCtx' with a '()' context.
type SpockAction conn sess st = SpockActionCtx () conn sess st

-- | Spock configuration, use 'defaultSpockCfg' and change single values if needed
data SpockCfg conn sess st = SpockCfg
  { -- | initial application global state
    spc_initialState :: st,
    -- | See 'PoolOrConn'
    spc_database :: PoolOrConn conn,
    -- | See 'SessionCfg'
    spc_sessionCfg :: SessionCfg conn sess st,
    -- | Maximum request size in bytes. 'Nothing' means no limit. Defaults to 5 MB in @defaultSpockCfg@.
    spc_maxRequestSize :: Maybe Word64,
    -- | Custom error handlers for implicit errors such as not matching routes or
    -- exceptions during a request handler run.
    spc_errorHandler :: Status -> ActionCtxT () IO (),
    -- | Function that should be called to log errors.
    spc_logError :: T.Text -> IO (),
    -- | When set to true, all non GET request will require
    -- either an HTTP-Header 'spc_csrfHeaderName' or a
    -- POST-Parameter 'spc_csrfPostName' to be set to the value aquired by 'getCsrfToken'
    spc_csrfProtection :: Bool,
    -- | see 'spc_csrfHeaderName'
    spc_csrfHeaderName :: T.Text,
    -- | see 'spc_csrfPostName'
    spc_csrfPostName :: T.Text
  }

-- | If Spock should take care of connection pooling, you need to configure
-- it depending on what you need.
data PoolCfg = PoolCfg
  { pc_stripes :: Int,
    pc_resPerStripe :: Int,
    pc_keepOpenTime :: NominalDiffTime
  }

-- | The ConnBuilder instructs Spock how to create or close a database connection.
data ConnBuilder a = ConnBuilder
  { cb_createConn :: IO a,
    cb_destroyConn :: a -> IO (),
    cb_poolConfiguration :: PoolCfg
  }

-- | You can feed Spock with either a connection pool, or instructions on how to build
-- a connection pool. See 'ConnBuilder'
data PoolOrConn a where
  PCPool :: Pool a -> PoolOrConn a
  PCConn :: ConnBuilder a -> PoolOrConn a
  PCNoDatabase :: PoolOrConn ()

-- | Configuration for the session manager
data SessionCfg conn a st = SessionCfg
  { -- | name of the client side cookie
    sc_cookieName :: T.Text,
    -- | how long the client side cookie should live
    sc_cookieEOL :: CookieEOL,
    -- | how long shoud a client session live
    sc_sessionTTL :: NominalDiffTime,
    -- | entropy of the session id sent to the client
    sc_sessionIdEntropy :: Int,
    -- | if this is true, every page reload will renew the session time to live counter
    sc_sessionExpandTTL :: Bool,
    -- | initial session for visitors
    sc_emptySession :: a,
    -- | storage interface for sessions
    sc_store :: SessionStoreInstance (Session conn a st),
    -- | how often should the session manager check for dangeling dead sessions
    sc_housekeepingInterval :: NominalDiffTime,
    -- | hooks into the session manager
    sc_hooks :: SessionHooks a
  }

-- | Hook into the session manager to trigger custom behavior
data SessionHooks a = SessionHooks
  { sh_removed :: HM.HashMap SessionId a -> IO ()
  }

data WebState conn sess st = WebState
  { web_dbConn :: Pool conn,
    web_sessionMgr :: SpockSessionManager conn sess st,
    web_state :: st,
    web_config :: SpockCfg conn sess st
  }

class HasSpock m where
  type SpockConn m :: *
  type SpockState m :: *
  type SpockSession m :: *

  -- | Give you access to a database connectin from the connection pool. The connection is
  -- released back to the pool once the function terminates.
  runQuery :: (SpockConn m -> IO a) -> m a

  -- | Read the application's state. If you wish to have mutable state, you could
  -- use a 'TVar' from the STM packge.
  getState :: m (SpockState m)

  -- | Get the session manager
  getSessMgr :: m (SpockSessionManager (SpockConn m) (SpockSession m) (SpockState m))

  -- | Get the Spock configuration
  getSpockCfg :: m (SpockCfg (SpockConn m) (SpockSession m) (SpockState m))

newtype WebStateT conn sess st m a = WebStateT {runWebStateT :: ReaderT (WebState conn sess st) m a}
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadIO,
      MonadReader (WebState conn sess st),
      MonadTrans
    )

instance MonadBase b m => MonadBase b (WebStateT conn sess st m) where
  liftBase = liftBaseDefault

instance MonadTransControl (WebStateT conn sess st) where
  type StT (WebStateT conn sess st) a = a
  liftWith = defaultLiftWith WebStateT runWebStateT
  restoreT = defaultRestoreT WebStateT

instance MonadBaseControl b m => MonadBaseControl b (WebStateT conn sess st m) where
  type StM (WebStateT conn sess st m) a = ComposeSt (WebStateT conn sess st) m a
  restoreM = defaultRestoreM
  liftBaseWith = defaultLiftBaseWith

type WebStateM conn sess st = WebStateT conn sess st (ResourceT IO)

type SessionId = T.Text

data Session conn sess st = Session
  { sess_id :: !SessionId,
    sess_csrfToken :: !T.Text,
    sess_validUntil :: !UTCTime,
    sess_data :: !sess
  }

data SessionStoreInstance sess where
  SessionStoreInstance :: forall sess tx. (Monad tx, Functor tx, Applicative tx) => SessionStore sess tx -> SessionStoreInstance sess

data SessionStore sess tx = SessionStore
  { ss_runTx :: forall a. tx a -> IO a,
    ss_loadSession :: SessionId -> tx (Maybe sess),
    ss_deleteSession :: SessionId -> tx (),
    ss_storeSession :: sess -> tx (),
    ss_toList :: tx [sess],
    ss_filterSessions :: (sess -> Bool) -> tx (),
    ss_mapSessions :: (sess -> tx sess) -> tx ()
  }

instance Show (Session conn sess st) where
  show = show . sess_id

type SpockSessionManager conn sess st = SessionManager (SpockActionCtx () conn sess st) conn sess st

data SessionManager m conn sess st = SessionManager
  { sm_getSessionId :: m SessionId,
    sm_getCsrfToken :: m T.Text,
    sm_regenerateSessionId :: m (),
    sm_readSession :: m sess,
    sm_writeSession :: sess -> m (),
    sm_modifySession :: forall a. (sess -> (sess, a)) -> m a,
    sm_mapSessions :: (forall n. Monad n => sess -> n sess) -> m (),
    sm_clearAllSessions :: MonadIO m => m (),
    sm_middleware :: Middleware,
    sm_closeSessionManager :: IO ()
  }
