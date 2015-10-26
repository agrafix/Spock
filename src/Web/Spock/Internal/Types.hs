{-# LANGUAGE CPP #-}
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

import Web.Spock.Internal.Wire

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif
import Control.Concurrent.STM
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Hashable
import Data.Pool
import Data.Time.Clock ( UTCTime(..), NominalDiffTime )
import Data.Typeable
import Data.Word
import Network.Wai
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

-- | Inside the SpockAllM monad, you may define routes and middleware.
type SpockAllM r conn sess st a =
    SpockAllT r (WebStateM conn sess st) a

-- | The 'SpockActionCtx' is the monad of all route-actions. You have access
-- to the context of the request and database, session and state of your application.
type SpockActionCtx ctx conn sess st = ActionCtxT ctx (WebStateM conn sess st)

-- | The 'SpockAction' is a specialisation of 'SpockActionCtx' with a '()' context.
type SpockAction conn sess st = SpockActionCtx () conn sess st

-- | Spock configuration, use 'defaultSpockCfg' and change single values if needed
data SpockCfg conn sess st
   = SpockCfg
   { spc_initialState :: st
     -- ^ initial application global state
   , spc_database :: PoolOrConn conn
     -- ^ See 'PoolOrConn'
   , spc_sessionCfg :: SessionCfg sess
     -- ^ See 'SessionCfg'
   , spc_maxRequestSize :: Maybe Word64
     -- ^ Maximum request size in bytes. 'Nothing' means no limit. Defaults to 5 MB in @defaultSpockCfg@.
   }

-- | Spock configuration with reasonable defaults
defaultSpockCfg :: sess -> PoolOrConn conn -> st -> SpockCfg conn sess st
defaultSpockCfg sess conn st =
    SpockCfg
    { spc_initialState = st
    , spc_database = conn
    , spc_sessionCfg = defaultSessionCfg sess
    , spc_maxRequestSize = Just (5 * 1024 * 1024)
    }

-- | If Spock should take care of connection pooling, you need to configure
-- it depending on what you need.
data PoolCfg
   = PoolCfg
   { pc_stripes :: Int
   , pc_resPerStripe :: Int
   , pc_keepOpenTime :: NominalDiffTime
   }

-- | The ConnBuilder instructs Spock how to create or close a database connection.
data ConnBuilder a
   = ConnBuilder
   { cb_createConn :: IO a
   , cb_destroyConn :: a -> IO ()
   , cb_poolConfiguration :: PoolCfg
   }

-- | You can feed Spock with either a connection pool, or instructions on how to build
-- a connection pool. See 'ConnBuilder'
data PoolOrConn a where
    PCPool :: Pool a -> PoolOrConn a
    PCConn :: ConnBuilder a -> PoolOrConn a
    PCNoDatabase :: PoolOrConn ()

-- | Session configuration with reasonable defaults
defaultSessionCfg :: a -> SessionCfg a
defaultSessionCfg emptySession =
    SessionCfg
    { sc_cookieName = "spockcookie"
    , sc_sessionTTL = 3600
    , sc_sessionIdEntropy = 64
    , sc_sessionExpandTTL = True
    , sc_emptySession = emptySession
    , sc_persistCfg = Nothing
    , sc_housekeepingInterval = 60 * 10
    , sc_hooks = defaultSessionHooks
    }

-- | Configuration for the session manager
data SessionCfg a
   = SessionCfg
   { sc_cookieName :: T.Text
     -- ^ name of the client side cookie
   , sc_sessionTTL :: NominalDiffTime
     -- ^ how long shoud a client session live
   , sc_sessionIdEntropy :: Int
     -- ^ entropy of the session id sent to the client
   , sc_sessionExpandTTL :: Bool
     -- ^ if this is true, every page reload will renew the session time to live counter
   , sc_emptySession :: a
     -- ^ initial session for visitors
   , sc_persistCfg :: Maybe (SessionPersistCfg a)
     -- ^ persistence interface for sessions
   , sc_housekeepingInterval :: NominalDiffTime
     -- ^ how often should the session manager check for dangeling dead sessions
   , sc_hooks :: SessionHooks a
     -- ^ hooks into the session manager
   }

-- | NOP session hooks
defaultSessionHooks :: SessionHooks a
defaultSessionHooks =
    SessionHooks
    { sh_removed = const $ return ()
    }

-- | Hook into the session manager to trigger custom behavior
data SessionHooks a
   = SessionHooks
   { sh_removed :: HM.HashMap SessionId a -> IO ()
   }

data SessionPersistCfg a
   = SessionPersistCfg
   { spc_load :: IO [(SessionId, UTCTime, a)]
   , spc_store :: [(SessionId, UTCTime, a)] -> IO ()
   }

data WebState conn sess st
   = WebState
   { web_dbConn :: Pool conn
   , web_sessionMgr :: SessionManager conn sess st
   , web_state :: st
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
    getSessMgr :: m (SessionManager (SpockConn m) (SpockSession m) (SpockState m))

-- | SafeActions are actions that need to be protected from csrf attacks
class (Hashable a, Eq a, Typeable a) => SafeAction conn sess st a where
    -- | The body of the safe action. Either GET or POST
    runSafeAction :: a -> SpockAction conn sess st ()

data PackedSafeAction conn sess st
    = forall a. (SafeAction conn sess st a) => PackedSafeAction { unpackSafeAction :: a }

instance Hashable (PackedSafeAction conn sess st) where
    hashWithSalt i (PackedSafeAction a) = hashWithSalt i a

instance Eq (PackedSafeAction conn sess st) where
   (PackedSafeAction a) == (PackedSafeAction b) =
       cast a == Just b

data SafeActionStore conn sess st
   = SafeActionStore
   { sas_forward :: !(HM.HashMap SafeActionHash (PackedSafeAction conn sess st))
   , sas_reverse :: !(HM.HashMap (PackedSafeAction conn sess st) SafeActionHash)
   }

type SafeActionHash = T.Text

newtype WebStateT conn sess st m a = WebStateT { runWebStateT :: ReaderT (WebState conn sess st) m a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadReader (WebState conn sess st), MonadTrans)

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
data Session conn sess st
    = Session
    { sess_id :: !SessionId
    , sess_validUntil :: !UTCTime
    , sess_data :: !sess
    , sess_safeActions :: !(SafeActionStore conn sess st)
    }

instance Show (Session conn sess st) where
    show = show . sess_id

data SessionManager conn sess st
   = SessionManager
   { sm_getSessionId :: forall ctx. SpockActionCtx ctx conn sess st SessionId
   , sm_regenerateSessionId :: forall ctx. SpockActionCtx ctx conn sess st ()
   , sm_readSession :: forall ctx. SpockActionCtx ctx conn sess st sess
   , sm_writeSession :: forall ctx. sess -> SpockActionCtx ctx conn sess st ()
   , sm_modifySession :: forall a ctx. (sess -> (sess, a)) -> SpockActionCtx ctx conn sess st a
   , sm_mapSessions :: forall ctx. (sess -> STM sess) -> SpockActionCtx ctx conn sess st ()
   , sm_clearAllSessions :: forall ctx. SpockActionCtx ctx conn sess st ()
   , sm_middleware :: Middleware
   , sm_addSafeAction :: forall ctx. PackedSafeAction conn sess st -> SpockActionCtx ctx conn sess st SafeActionHash
   , sm_lookupSafeAction :: forall ctx. SafeActionHash -> SpockActionCtx ctx conn sess st (Maybe (PackedSafeAction conn sess st))
   , sm_removeSafeAction :: forall ctx. PackedSafeAction conn sess st -> SpockActionCtx ctx conn sess st ()
   , sm_closeSessionManager :: IO ()
   }
