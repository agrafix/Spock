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
import Control.Exception (Exception)
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString as BS
import qualified Data.Vault.Lazy as V
import Data.Pool
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime (..))
import Data.Word
import Network.HTTP.Types.Status
import Network.Wai
import Web.Spock.Core

-- | Inside the SpockAllM monad, you may define routes and middleware.
type SpockAllM conn sess st a = SpockT (WebStateM conn sess st) a

-- | A per-request action with typed context @ctx@, database connection @conn@,
-- session value @sess@, and shared application state @st@. The final type
-- parameter is the action's result. Use @getContext@ for the value supplied by
-- a prehook, 'getState' for shared state, and session actions for this visitor.
--
-- This is 'ActionCtxT' over 'WebStateM'. @lift helper@ enters 'WebStateM';
-- @liftIO operation@ runs IO. Sending a response finishes the action even
-- though response helpers have a polymorphic result type.
type SpockActionCtx ctx conn sess st = ActionCtxT ctx (WebStateM conn sess st)

-- | A per-request 'SpockActionCtx' with context @()@. This is the usual handler
-- type outside a prehook. Its @conn@, @sess@, and @st@ parameters have the same
-- meanings as in the application's route-registration type.
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
    -- | Optional request IDs and structured handler/access/error logging.
    spc_logging :: Maybe LoggingConfig,
    -- | Slash matching and optional 308 canonical redirects. Defaults to
    -- 'IgnoreSlashes'; set 'StrictSlashes' to distinguish @/foo@ and @/foo/@.
    spc_slashPolicy :: SlashPolicy,
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
  { -- | When to load or create a session. Defaults to 'SessionsOnDemand'.
    sc_sessionMode :: SessionMode,
    -- | name of the client side cookie
    sc_cookieName :: T.Text,
    -- | how long the client side cookie should live
    sc_cookieSettings :: CookieSettings,
    -- | how long shoud a client session live
    sc_sessionTTL :: NominalDiffTime,
    -- | entropy of the session id sent to the client
    sc_sessionIdEntropy :: Int,
    -- | if this is true, every page reload will renew the session time to live counter
    sc_sessionExpandTTL :: Bool,
    -- | initial session for visitors
    sc_emptySession :: a,
    -- | Server storage or an authenticated client-cookie codec. Client sessions
    -- do not have server-wide mapping, deletion, or housekeeping capabilities.
    sc_backend :: SessionBackend conn a st
  }

-- | Select storage separately from the shared lifetime, cookie and mode options.
data SessionBackend conn sess st
  = ServerSessions (ServerSessionCfg conn sess st)
  | ClientSessions (ClientSessionCfg sess)

-- | Store, sweep interval and removal hooks used only by server backends.
-- Customize the value returned by @defaultServerSessionCfg@.
data ServerSessionCfg conn sess st = ServerSessionCfg
  { ssc_store :: SessionStoreInstance (Session conn sess st),
    ssc_housekeepingInterval :: NominalDiffTime,
    ssc_hooks :: SessionHooks sess
  }

-- | Trusted codec interface for backend packages. Encode must authenticate and
-- encrypt the complete session, bound to the supplied cookie name. Decode must
-- authenticate before returning data, and return Nothing for untrusted input.
-- The Bool asks the manager to reissue a cookie after key or format rotation.
data ClientSessionCodec sess = ClientSessionCodec
  { csc_encode :: forall conn st. T.Text -> Session conn sess st -> IO BS.ByteString,
    csc_decode :: forall conn st. T.Text -> BS.ByteString -> IO (Maybe (Session conn sess st, Bool))
  }

-- | Cookie backend settings. Use a maintained authenticated codec such as
-- @Spock-session-cookie@; plain JSON or unauthenticated encryption is unsafe.
data ClientSessionCfg sess = ClientSessionCfg
  { csc_codec :: ClientSessionCodec sess,
    -- | Limit for the complete Set-Cookie value, including name and attributes.
    -- Must be between 1 and 4096. Oversized writes fail before changing state.
    csc_maxCookieBytes :: Int,
    -- | Server clock. Defaults to getCurrentTime; replace in deterministic tests.
    csc_clock :: IO UTCTime
  }

-- | On-demand sessions are loaded only by session actions (including CSRF
-- checks). Disabled sessions leave database pooling and application state usable.
data SessionMode = SessionsAlways | SessionsOnDemand | SessionsDisabled
  deriving (Eq, Show)

-- | Invalid session configuration, unavailable capabilities or rejected writes.
data SessionError = SessionUseWhenDisabled | CsrfRequiresSessions
  | ServerSessionsRequired | ClientSessionOutsideRequest
  | ClientSessionCookieTooLarge | InvalidClientSessionConfig
  deriving (Eq, Show)

instance Exception SessionError

-- | Hook into the session manager to trigger custom behavior
data SessionHooks a = SessionHooks
  { sh_removed :: HM.HashMap SessionId a -> IO ()
  }

-- | The application environment: connection pool, session manager,
-- configuration, and shared state. It does not contain a particular request.
data WebState conn sess st = WebState
  { web_dbConn :: Pool conn,
    web_sessionMgr :: SpockSessionManager conn sess st,
    web_state :: st,
    web_config :: SpockCfg conn sess st
  }

-- | Access application services in 'WebStateM' and Spock's registration/action
-- layers. Helpers polymorphic in this class can use 'getState' and 'runQuery'
-- without committing to either of those layers.
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

-- | Add the application's 'WebState' environment to an underlying monad @m@.
-- @conn@, @sess@, and @st@ select the connection, session value, and shared state
-- types; @a@ is the computation's result. This is a reader of an existing
-- environment, not a mutable state transformer. Put an @IORef@ or @TVar@ in
-- @st@ when shared state needs to change, with suitable synchronization.
--
-- Applications normally use 'WebStateM', the resource-managed IO specialization.
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

-- | Shared application services over @ResourceT IO@. This is the base monad
-- underneath both route registration and request actions in full Spock.
-- Helpers here can use 'getState' and 'runQuery', but have no request body,
-- response, hook context, or current visitor's session. Those require the
-- action layer. Lift such a helper into an action or registration block with
-- @lift@; use @runSpockIO@ with an existing environment to run it from IO.
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
  { -- | Run the entire supplied transaction atomically, isolated from other
    -- transactions. Session lookup, expiration, renewal, and modification rely
    -- on this guarantee to avoid lost updates or restoring deleted sessions.
    ss_runTx :: forall a. tx a -> IO a,
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

data SessionIf m = SessionIf
  { si_queryVault :: forall a. V.Key a -> m (Maybe a),
    si_modifyVault :: (V.Vault -> V.Vault) -> m (),
    si_setRawMultiHeader :: MultiHeader -> BS.ByteString -> m (),
    si_vaultKey :: IO (V.Key SessionId)
  }

data SessionManager m conn sess st = SessionManager
  { sm_getSessionId :: m SessionId,
    sm_getCsrfToken :: m T.Text,
    sm_regenerateSessionId :: m (),
    sm_destroySession :: m (),
    sm_readSession :: m sess,
    sm_writeSession :: sess -> m (),
    sm_modifySession :: forall a. (sess -> (sess, a)) -> m a,
    sm_serverSessions :: Maybe (ServerSessionManager m sess),
    sm_middleware :: Middleware,
    sm_closeSessionManager :: IO ()
  }

-- | Explicit capability available only for enabled server-backed sessions.
-- Public actions in Web.Spock.SessionActions.Server require this handle.
data ServerSessionManager m sess = ServerSessionManager
  { ssm_mapSessions :: (forall n. Monad n => sess -> n sess) -> m (),
    ssm_clearAllSessions :: m ()
  }
