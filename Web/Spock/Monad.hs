{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Spock.Monad where

import Web.Spock.Types
import Web.Spock.SessionManager

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Pool
import Data.Time.Clock ( UTCTime(..) )
import Web.Scotty.Trans
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Text.XML.XSD.DateTime as XSD

webM :: MonadTrans t => WebStateM conn sess st a -> t (WebStateM conn sess st) a
webM = lift

runQuery :: MonadTrans t => (conn -> IO a) -> t (WebStateM conn sess st) a
runQuery query =
    webM $
    do pool <- asks web_dbConn
       liftIO $ withResource pool query

getState :: MonadTrans t => t (WebStateM conn sess st) st
getState = webM $ asks web_state

getSessMgr :: MonadTrans t => t (WebStateM conn sess st) (SessionManager sess)
getSessMgr = webM $ asks web_sessionMgr

instance Parsable T.Text where
    parseParam = Right . TL.toStrict

instance Parsable BSL.ByteString where
    parseParam = Right . BSL.fromStrict . T.encodeUtf8 . TL.toStrict

instance Parsable UTCTime where
    parseParam p =
        case join $ fmap XSD.toUTCTime $ XSD.dateTime (TL.toStrict p) of
          Nothing -> Left $ TL.pack $ "Can't parse param (`" ++ show p ++ "`) as UTCTime!"
          Just x -> Right x

instance (Functor a, Monad a, Applicative a) => Applicative (ActionT a) where
    pure = return
    (<*>) = ap
