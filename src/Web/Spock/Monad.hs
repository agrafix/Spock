{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Web.Spock.Monad where

import Web.Spock.Types

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Pool
import Data.Time.Clock ( UTCTime(..) )
import Web.PathPieces
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.XML.XSD.DateTime as XSD

webM :: MonadTrans t => WebStateM conn sess st a -> t (WebStateM conn sess st) a
webM = lift

instance MonadTrans t => HasSpock (t (WebStateM conn sess st)) where
    type SpockConn (t (WebStateM conn sess st)) = conn
    type SpockState (t (WebStateM conn sess st)) = st
    type SpockSession (t (WebStateM conn sess st)) = sess
    runQuery a = webM $ runQueryImpl a
    getState = webM $ getStateImpl
    getSessMgr = webM $ getSessMgrImpl

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
       liftIO (withResource pool $ query)

getStateImpl :: WebStateM conn sess st st
getStateImpl = asks web_state

-- | Read the heart of Spock. This is useful if you want to construct your own
-- monads that work with runQuery and getState using "runSpockIO"
getSpockHeart :: MonadTrans t => t (WebStateM conn sess st) (WebState conn sess st)
getSpockHeart = webM ask

-- | Run an action inside of Spocks core monad. This allows you to use runQuery and getState
runSpockIO :: WebState conn sess st -> WebStateM conn sess st a -> IO a
runSpockIO st (WebStateM action) =
    runResourceT $
    runReaderT action st

getSessMgrImpl :: (WebStateM conn sess st) (SessionManager conn sess st)
getSessMgrImpl = asks web_sessionMgr

instance PathPiece BSL.ByteString where
    fromPathPiece =
        Just . BSL.fromStrict . T.encodeUtf8
    toPathPiece =
        T.decodeUtf8 . BSL.toStrict

instance PathPiece UTCTime where
    fromPathPiece p = join $ fmap XSD.toUTCTime $ XSD.dateTime p
    toPathPiece = T.pack . show . XSD.fromUTCTime
