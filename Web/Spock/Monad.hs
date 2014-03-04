{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Web.Spock.Monad where

import Web.Spock.Types

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Pool
import Data.Time.Clock ( UTCTime(..) )
import Web.Scotty.Trans
import qualified Data.Conduit.Pool as CP
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Text.XML.XSD.DateTime as XSD

webM :: MonadTrans t => WebStateM conn sess st a -> t (WebStateM conn sess st) a
webM = lift

class HasSpock conn st m where
    -- | Give you access to a database connectin from the connection pool. The connection is
    -- released back to the pool once the function terminates.
    runQuery :: (conn -> IO a) -> m a
    -- | Read the application's state. If you wish to have mutable state, you could
    -- use a 'TVar' from the STM packge.
    getState :: m st

instance MonadTrans t => HasSpock conn st (t (WebStateM conn sess st)) where
    runQuery a = webM $ runQueryImpl a
    getState = webM $ getStateImpl

instance HasSpock conn st (WebStateM conn sess st) where
    runQuery = runQueryImpl
    getState = getStateImpl

runQueryImpl :: (conn -> IO a) -> WebStateM conn sess st a
runQueryImpl query =
    do pool <- asks web_dbConn
       let fun =
               case pool of
                 DataPool p ->
                     withResource p
                 ConduitPool p ->
                     CP.withResource p
       liftIO (fun query)

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

getSessMgr :: MonadTrans t => t (WebStateM conn sess st) (SessionManager sess)
getSessMgr = webM $ asks web_sessionMgr

instance Parsable BSL.ByteString where
    parseParam =
        Right . BSL.fromStrict . T.encodeUtf8 . TL.toStrict

instance Parsable UTCTime where
    parseParam p =
        case join $ fmap XSD.toUTCTime $ XSD.dateTime (TL.toStrict p) of
          Nothing ->
              Left $ TL.pack $ "Can't parse param (`" ++ show p ++ "`) as UTCTime!"
          Just x ->
              Right x
