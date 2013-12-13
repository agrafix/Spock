{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Spock.Types where

import Web.Scotty.Trans

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Pool
import Data.Time.Clock ( UTCTime(..) )
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

type SpockM conn sess st a = ScottyT (WebStateM conn sess st) a
type SpockAction conn sess st a = ActionT (WebStateM conn sess st) a

data StorageLayer a
   = StorageLayer
   { sl_createConn :: IO a
   , sl_closeConn :: a -> IO ()
   }

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
   , sm_sessionFromCookie :: MonadIO m => ActionT m (Maybe (Session a))
   , sm_createCookieSession :: MonadIO m => a -> ActionT m ()
   , sm_newSession :: a -> IO (Session a)
   , sm_deleteSession :: SessionId -> IO ()
   }
