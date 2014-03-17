{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
module Web.Spock.Types where

import Web.Scotty.Trans

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Hashable
import Data.Pool
import Data.Text.Lazy (Text)
import Data.Time.Clock ( UTCTime(..), NominalDiffTime )
import Data.Typeable
import Network.Wai
import qualified Data.Conduit.Pool as CP
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

type SpockError e = ScottyError e

-- | Spock is supercharged Scotty, that's why the 'SpockM' is built on the
-- ScottyT monad. Insive the SpockM monad, you may define routes and middleware.
type SpockM conn sess st a = ScottyT Text (WebStateM conn sess st) a

-- | The SpockAction is the monad of all route-actions. You have access
-- to the database, session and state of your application.
type SpockAction conn sess st a = ActionT Text (WebStateM conn sess st) a

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
data PoolOrConn a
   = PCPool (Pool a)
   | PCConduitPool (CP.Pool a)
   | PCConn (ConnBuilder a)

-- | Configuration for the session manager
data SessionCfg a
   = SessionCfg
   { sc_cookieName :: T.Text
   , sc_sessionTTL :: NominalDiffTime
   , sc_sessionIdEntropy :: Int
   , sc_emptySession :: a
   }

data ConnectionPool conn
   = DataPool (Pool conn)
   | ConduitPool (CP.Pool conn)

data WebState conn sess st
   = WebState
   { web_dbConn :: ConnectionPool conn
   , web_sessionMgr :: SessionManager sess
   , web_state :: st
   }

-- | SafeActions are actions that need to be protected from csrf attacks
class (Hashable a, Eq a, Typeable a) => SafeAction a where
    runSafeAction :: a -> SpockAction conn sess st ()

data PackedSafeAction
    = forall a. (SafeAction a) => PackedSafeAction { unpackSafeAction :: a }

instance Hashable PackedSafeAction where
    hashWithSalt i (PackedSafeAction a) = hashWithSalt i a

instance Eq PackedSafeAction where
   (PackedSafeAction a) == (PackedSafeAction b) =
       cast a == Just b

data SafeActionStore
   = SafeActionStore
   { sas_forward :: HM.HashMap SafeActionHash PackedSafeAction
   , sas_reverse :: HM.HashMap PackedSafeAction SafeActionHash
   }

type SafeActionHash = T.Text

newtype WebStateM conn sess st a = WebStateM { runWebStateM :: ReaderT (WebState conn sess st) (ResourceT IO) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadReader (WebState conn sess st))

instance MonadBase IO (WebStateM conn sess st) where
    liftBase = WebStateM . liftBase

instance MonadBaseControl IO (WebStateM conn sess st) where
    newtype StM (WebStateM conn sess st) a = WStM { unWStM :: StM (ReaderT (WebState conn sess st) (ResourceT IO)) a }
    liftBaseWith f = WebStateM . liftBaseWith $ \runInBase -> f $ liftM WStM . runInBase . runWebStateM
    restoreM = WebStateM . restoreM . unWStM

type SessionId = T.Text
data Session a
    = Session
    { sess_id :: SessionId
    , sess_validUntil :: UTCTime
    , sess_data :: a
    , sess_safeActions :: SafeActionStore
    }
type UserSessions a = TVar (HM.HashMap SessionId (Session a))

data SessionManager a
   = SessionManager
   { sm_readSession :: (SpockError e, MonadIO m) => ActionT e m a
   , sm_writeSession :: (SpockError e, MonadIO m) => a -> ActionT e m ()
   , sm_modifySession :: (SpockError e, MonadIO m) => (a -> a) -> ActionT e m ()
   , sm_middleware :: Middleware
   , sm_addSafeAction :: (SpockError e, MonadIO m) => PackedSafeAction -> ActionT e m SafeActionHash
   , sm_lookupSafeAction :: (SpockError e, MonadIO m) => SafeActionHash -> ActionT e m (Maybe PackedSafeAction)
   }
