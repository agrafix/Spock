{-# LANGUAGE FlexibleContexts, DeriveGeneric, OverloadedStrings, DoAndIfThenElse, RankNTypes #-}
module Web.Spock.SessionManager
    ( openSessionManager
    , SessionId, Session(..), SessionManager(..)
    )
where

import Web.Spock.Types
import Web.Spock.Cookie

import Control.Concurrent.STM
import Control.Monad.Trans
import Data.Time
import System.Random
import Web.Scotty.Trans
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64 as B64
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as T


openSessionManager :: SessionCfg -> IO (SessionManager a)
openSessionManager cfg =
    do cacheHM <- atomically $ newTVar HM.empty
       return $ SessionManager
                  { sm_loadSession = loadSessionImpl cfg cacheHM
                  , sm_sessionFromCookie = sessionFromCookieImpl cfg cacheHM
                  , sm_createCookieSession = createCookieSessionImpl cfg cacheHM
                  , sm_newSession = newSessionImpl cfg cacheHM
                  , sm_deleteSession = deleteSessionImpl cacheHM
                  }

createCookieSessionImpl :: (SpockError e, MonadIO m)
                        => SessionCfg
                        -> UserSessions a
                        -> a
                        -> ActionT e m ()
createCookieSessionImpl sessCfg sessRef val =
    do sess <- liftIO $ newSessionImpl sessCfg sessRef val
       setCookie' (sc_cookieName sessCfg) (sess_id sess) (sess_validUntil sess)

newSessionImpl :: SessionCfg
               -> UserSessions a
               -> a
               -> IO (Session a)
newSessionImpl sessCfg sessionRef content =
    do sess <- createSession sessCfg content
       atomically $ modifyTVar sessionRef (\hm -> HM.insert (sess_id sess) sess hm)
       return sess

sessionFromCookieImpl :: (SpockError e, MonadIO m)
                      => SessionCfg
                      -> UserSessions a
                      -> ActionT e m (Maybe (Session a))
sessionFromCookieImpl sessCfg sessionRef =
    do mSid <- getCookie (sc_cookieName sessCfg)
       case mSid of
         Just sid ->
             liftIO $ loadSessionImpl sessCfg sessionRef sid
         Nothing ->
             return Nothing

loadSessionImpl :: SessionCfg
                -> UserSessions a
                -> SessionId
                -> IO (Maybe (Session a))
loadSessionImpl sessCfg sessionRef sid =
    do sessHM <- atomically $ readTVar sessionRef
       now <- getCurrentTime
       case HM.lookup sid sessHM of
         Just sess ->
             do if addUTCTime (sc_sessionTTL sessCfg) (sess_validUntil sess) > now
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

createSession :: SessionCfg -> a -> IO (Session a)
createSession sessCfg content =
    do gen <- g
       let sid = T.decodeUtf8 $ B64.encode $ BSC.pack $
                 take (sc_sessionIdEntropy sessCfg) $ randoms gen
       now <- getCurrentTime
       let validUntil = addUTCTime (sc_sessionTTL sessCfg) now
       return (Session sid validUntil content)
    where
      g = newStdGen :: IO StdGen
