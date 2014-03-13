{-# LANGUAGE FlexibleContexts, DeriveGeneric, OverloadedStrings, DoAndIfThenElse, RankNTypes #-}
module Web.Spock.SessionManager
    ( createSessionManager
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
import qualified Data.Vault.Lazy as V
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64 as B64
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as T
import qualified Network.Wai as Wai
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Wai.Util as Wai


createSessionManager :: SessionCfg a -> IO (SessionManager a)
createSessionManager cfg =
    do cacheHM <- atomically $ newTVar HM.empty
       vaultKey <- V.newKey
       return $ SessionManager
                  { sm_readSession = readSessionImpl vaultKey cacheHM
                  , sm_writeSession = writeSessionImpl vaultKey cacheHM
                  , sm_modifySession = modifySessionImpl vaultKey cacheHM
                  , sm_middleware = sessionMiddleware cfg vaultKey cacheHM
                  }

readSessionImpl :: (SpockError e, MonadIO m)
                => V.Key SessionId
                -> UserSessions a
                -> ActionT e m a
readSessionImpl vK sessionRef =
    do req <- request
       case V.lookup vK (Wai.vault req) of
         Nothing ->
             error "(1) Internal Spock Session Error. Please report this bug!"
         Just sid ->
             do sessions <- liftIO $ atomically $ readTVar sessionRef
                case HM.lookup sid sessions of
                  Nothing ->
                      error "(2) Internal Spock Session Error. Please report this bug!"
                  Just session ->
                      return (sess_data session)

writeSessionImpl :: (SpockError e, MonadIO m)
                 => V.Key SessionId
                 -> UserSessions a
                 -> a
                 -> ActionT e m ()
writeSessionImpl vK sessionRef value =
    modifySessionImpl vK sessionRef (const value)

modifySessionImpl :: (SpockError e, MonadIO m)
                  => V.Key SessionId
                  -> UserSessions a
                  -> (a -> a)
                  -> ActionT e m ()
modifySessionImpl vK sessionRef f =
    do req <- request
       case V.lookup vK (Wai.vault req) of
         Nothing ->
             error "(3) Internal Spock Session Error. Please report this bug!"
         Just sid ->
             do let modFun session =
                        session { sess_data = f (sess_data session) }
                liftIO $ atomically $ modifyTVar sessionRef (HM.adjust modFun sid)


sessionMiddleware :: SessionCfg a
                  -> V.Key SessionId
                  -> UserSessions a
                  -> Wai.Middleware
sessionMiddleware cfg vK sessionRef app req =
    case getCookieFromReq (sc_cookieName cfg) req of
      Just sid ->
          do mSess <- loadSessionImpl cfg sessionRef sid
             case mSess of
               Nothing ->
                   mkNew
               Just sess ->
                   withSess sess
      Nothing ->
          mkNew
    where
      defVal = sc_emptySession cfg
      v = Wai.vault req
      addCookie sess responseHeaders =
          let cookieContent =
                  renderCookie (sc_cookieName cfg) (sess_id sess) (sess_validUntil sess)
              cookie = ("Set-Cookie", BSL.toStrict $ TL.encodeUtf8 cookieContent)
          in (cookie : responseHeaders)
      withSess sess =
          do resp <- app (req { Wai.vault = V.insert vK (sess_id sess) v })
             return $ Wai.mapHeaders (addCookie sess) resp
      mkNew =
          do newSess <- newSessionImpl cfg sessionRef defVal
             withSess newSess

newSessionImpl :: SessionCfg a
               -> UserSessions a
               -> a
               -> IO (Session a)
newSessionImpl sessCfg sessionRef content =
    do sess <- createSession sessCfg content
       atomically $ modifyTVar sessionRef (\hm -> HM.insert (sess_id sess) sess hm)
       return sess

loadSessionImpl :: SessionCfg a
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

createSession :: SessionCfg a -> a -> IO (Session a)
createSession sessCfg content =
    do gen <- g
       let sid = T.decodeUtf8 $ B64.encode $ BSC.pack $
                 take (sc_sessionIdEntropy sessCfg) $ randoms gen
       now <- getCurrentTime
       let validUntil = addUTCTime (sc_sessionTTL sessCfg) now
       return (Session sid validUntil content)
    where
      g = newStdGen :: IO StdGen
