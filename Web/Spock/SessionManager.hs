{-# LANGUAGE FlexibleContexts, DeriveGeneric, OverloadedStrings, DoAndIfThenElse, RankNTypes #-}
module Web.Spock.SessionManager
    ( openSessionManager
    , SessionId, Session(..), SessionManager(..)
    )
where

import Control.Arrow
import Control.Concurrent.STM
import Control.Monad.Trans
import Crypto.Random (newGenIO, genBytes, SystemRandom)
import Data.Time
import System.Locale
import Web.Scotty.Trans
import qualified Data.ByteString.Base64 as B64
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Network.Wai as Wai

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

_COOKIE_NAME_ = "asession"

sessionTTL = 10 * 60 * 60
sessionIdEntropy = 12 :: Int

openSessionManager :: IO (SessionManager a)
openSessionManager =
    do cacheHM <- atomically $ newTVar HM.empty
       return $ SessionManager
                  { sm_loadSession = loadSessionImpl cacheHM
                  , sm_sessionFromCookie = sessionFromCookieImpl cacheHM
                  , sm_createCookieSession = createCookieSessionImpl cacheHM
                  , sm_newSession = newSessionImpl cacheHM
                  , sm_deleteSession = deleteSessionImpl cacheHM
                  }

createCookieSessionImpl :: MonadIO m
                        => UserSessions a
                        -> a
                        -> ActionT m ()
createCookieSessionImpl sessRef val =
    do sess <- liftIO $ newSessionImpl sessRef val
       let formattedExp = T.pack $ formatTime defaultTimeLocale "%a, %d-%b-%Y %X %Z" (sess_validUntil sess)
       setHeader "Set-Cookie" (TL.concat [ TL.fromStrict _COOKIE_NAME_
                                         , "="
                                         , TL.fromStrict (sess_id sess)
                                         , "; path=/; expires="
                                         , TL.fromStrict formattedExp
                                         , ";"
                                         ]
                              )

newSessionImpl :: UserSessions a
                -> a
                -> IO (Session a)
newSessionImpl sessionRef content =
    do sess <- createSession content
       atomically $ modifyTVar sessionRef (\hm -> HM.insert (sess_id sess) sess hm)
       return sess

sessionFromCookieImpl :: MonadIO m
                      => UserSessions a -> ActionT m (Maybe (Session a))
sessionFromCookieImpl sessionRef =
    do req <- request
       case lookup "cookie" (Wai.requestHeaders req) >>=
            lookup _COOKIE_NAME_ . parseCookies . T.decodeUtf8 of
         Just sid ->
             liftIO $ loadSessionImpl sessionRef sid
         Nothing ->
             return Nothing
    where
      parseCookies :: T.Text -> [(T.Text, T.Text)]
      parseCookies = map parseCookie . T.splitOn ";" . T.concat . T.words

      parseCookie = first T.init . T.breakOnEnd "="

loadSessionImpl :: UserSessions a
                -> SessionId
                -> IO (Maybe (Session a))
loadSessionImpl sessionRef sid =
    do sessHM <- atomically $ readTVar sessionRef
       now <- getCurrentTime
       case HM.lookup sid sessHM of
         Just sess ->
             do if addUTCTime sessionTTL (sess_validUntil sess) > now
                then return $ Just sess
                else do deleteSessionImpl sessionRef sid
                        return Nothing
         Nothing ->
             return Nothing

deleteSessionImpl :: UserSessions a
                  -> SessionId
                  -> IO ()
deleteSessionImpl sessionRef sid =
    do atomically $ modifyTVar sessionRef (\hm -> HM.delete sid hm)
       return ()

createSession :: a -> IO (Session a)
createSession content =
    do gen <- g
       sid <- case genBytes sessionIdEntropy gen of
                Left err -> fail $ show err
                Right (x, _) -> return $ T.decodeUtf8 $ B64.encode x
       now <- getCurrentTime
       let validUntil = addUTCTime sessionTTL now
       return (Session sid validUntil content)
    where
      g = newGenIO :: IO SystemRandom
