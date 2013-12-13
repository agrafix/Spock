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
       setCookie' _COOKIE_NAME_ (sess_id sess) (sess_validUntil sess)

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
    do mSid <- getCookie _COOKIE_NAME_
       case mSid of
         Just sid ->
             liftIO $ loadSessionImpl sessionRef sid
         Nothing ->
             return Nothing

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
       let sid = T.decodeUtf8 $ B64.encode $ BSC.pack $
                 take sessionIdEntropy $ randoms gen
       now <- getCurrentTime
       let validUntil = addUTCTime sessionTTL now
       return (Session sid validUntil content)
    where
      g = newStdGen :: IO StdGen
