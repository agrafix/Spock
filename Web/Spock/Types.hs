{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module Web.Spock.Types where

import Web.Scotty.Trans

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Pool
import Data.Time.Clock ( UTCTime(..), NominalDiffTime(..) )
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Text.Lazy (Text)

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
   | PCConn (ConnBuilder a)

-- | Configuration for the session manager
data SessionCfg
   = SessionCfg
   { sc_cookieName :: T.Text
   , sc_sessionTTL :: NominalDiffTime
   , sc_sessionIdEntropy :: Int
   }

-- | Assign the current session roles/permission, eg. admin or user
type UserRights = T.Text

-- | Describes why access was denied to a user
data NoAccessReason
   = NotEnoughRights
   | NotLoggedIn
   | NoSession
   deriving (Show, Eq, Read, Enum)

data WebState conn sess st
   = WebState
   { web_dbConn :: Pool conn
   , web_sessionMgr :: SessionManager sess
   , web_state :: st
   }

newtype WebStateM conn sess st a = WebStateM { runWebStateM :: ReaderT (WebState conn sess st) (ResourceT IO) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadReader (WebState conn sess st))

type SessionId = T.Text
data Session a
    = Session
    { sess_id :: SessionId
    , sess_validUntil :: UTCTime
    , sess_data :: a
    }
type UserSessions a = TVar (HM.HashMap SessionId (Session a))

data SessionManager a
   = SessionManager
   { sm_loadSession :: SessionId -> IO (Maybe (Session a))
   , sm_sessionFromCookie :: (SpockError e, MonadIO m)
                          => ActionT e m (Maybe (Session a))
   , sm_createCookieSession :: (SpockError e, MonadIO m) => a -> ActionT e m ()
   , sm_newSession :: a -> IO (Session a)
   , sm_deleteSession :: SessionId -> IO ()
   }
