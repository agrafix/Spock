{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Web.Spock.Internal.Monad where

import Web.Spock.Internal.Types

import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Pool

webM :: MonadTrans t => WebStateM conn sess st a -> t (WebStateM conn sess st) a
webM = lift

instance MonadTrans t => HasSpock (t (WebStateM conn sess st)) where
    type SpockConn (t (WebStateM conn sess st)) = conn
    type SpockState (t (WebStateM conn sess st)) = st
    type SpockSession (t (WebStateM conn sess st)) = sess
    runQuery a = webM $ runQueryImpl a
    getState = webM getStateImpl
    getSessMgr = webM getSessMgrImpl

instance HasSpock (WebStateM conn sess st) where
    type SpockConn (WebStateM conn sess st) = conn
    type SpockState (WebStateM conn sess st) = st
    type SpockSession (WebStateM conn sess st) = sess
    runQuery = runQueryImpl
    getState = getStateImpl
    getSessMgr = getSessMgrImpl

runQueryImpl :: (conn -> IO a) -> WebStateM conn sess st a
runQueryImpl query =
    do pool <- asks web_dbConn
       liftIO (withResource pool query)

getStateImpl :: WebStateM conn sess st st
getStateImpl = asks web_state

-- | Read the heart of Spock. This is useful if you want to construct your own
-- monads that work with runQuery and getState using "runSpockIO"
getSpockHeart :: MonadTrans t => t (WebStateM conn sess st) (WebState conn sess st)
getSpockHeart = webM ask

-- | Run an action inside of Spocks core monad. This allows you to use runQuery and getState
runSpockIO :: WebState conn sess st -> WebStateM conn sess st a -> IO a
runSpockIO st (WebStateT action) =
    runResourceT $
    runReaderT action st

getSessMgrImpl :: (WebStateM conn sess st) (SessionManager conn sess st)
getSessMgrImpl = asks web_sessionMgr
