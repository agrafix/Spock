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
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Vault.Lazy as V
import qualified Network.Wai as Wai
import qualified Network.Wai.Util as Wai

createSessionManager :: SessionCfg sess -> IO (SessionManager conn sess st)
createSessionManager cfg =
    do cacheHM <- atomically $ newTVar HM.empty
       vaultKey <- V.newKey
       return $ SessionManager
                  { sm_readSession = readSessionImpl vaultKey cacheHM
                  , sm_writeSession = writeSessionImpl vaultKey cacheHM
                  , sm_modifySession = modifySessionImpl vaultKey cacheHM
                  , sm_middleware = sessionMiddleware cfg vaultKey cacheHM
                  , sm_addSafeAction = addSafeActionImpl vaultKey cacheHM
                  , sm_lookupSafeAction = lookupSafeActionImpl vaultKey cacheHM
                  }

modifySessionBase :: V.Key SessionId
                  -> UserSessions conn sess st
                  -> (Session conn sess st -> Session conn sess st)
                  -> SpockAction conn sess st ()
modifySessionBase vK sessionRef modFun =
    do req <- request
       case V.lookup vK (Wai.vault req) of
         Nothing ->
             error "(3) Internal Spock Session Error. Please report this bug!"
         Just sid ->
             liftIO $ atomically $ modifyTVar sessionRef (HM.adjust modFun sid)

readSessionBase :: V.Key SessionId
                -> UserSessions conn sess st
                -> SpockAction conn sess st (Session conn sess st)
readSessionBase vK sessionRef =
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
                      return session

addSafeActionImpl :: V.Key SessionId
                  -> UserSessions conn sess st
                  -> PackedSafeAction conn sess st
                  -> SpockAction conn sess st SafeActionHash
addSafeActionImpl vaultKey cacheHM safeAction =
    do base <- readSessionBase vaultKey cacheHM
       case HM.lookup safeAction (sas_reverse (sess_safeActions base)) of
         Just safeActionHash ->
             return safeActionHash
         Nothing ->
             do safeActionHash <- liftIO (randomHash 40)
                let f sas =
                        sas
                        { sas_forward = HM.insert safeActionHash safeAction (sas_forward sas)
                        , sas_reverse = HM.insert safeAction safeActionHash (sas_reverse sas)
                        }
                modifySessionBase vaultKey cacheHM (\s -> s { sess_safeActions = f (sess_safeActions s) })
                return safeActionHash

lookupSafeActionImpl :: V.Key SessionId
                     -> UserSessions conn sess st
                     -> SafeActionHash
                     -> SpockAction conn sess st (Maybe (PackedSafeAction conn sess st))
lookupSafeActionImpl vaultKey cacheHM hash =
    do base <- readSessionBase vaultKey cacheHM
       return $ HM.lookup hash (sas_forward (sess_safeActions base))

readSessionImpl :: V.Key SessionId
                -> UserSessions conn sess st
                -> SpockAction conn sess st sess
readSessionImpl vK sessionRef =
    do base <- readSessionBase vK sessionRef
       return (sess_data base)

writeSessionImpl :: V.Key SessionId
                 -> UserSessions conn sess st
                 -> sess
                 -> SpockAction conn sess st ()
writeSessionImpl vK sessionRef value =
    modifySessionImpl vK sessionRef (const value)

modifySessionImpl :: V.Key SessionId
                  -> UserSessions conn sess st
                  -> (sess -> sess)
                  -> SpockAction conn sess st ()
modifySessionImpl vK sessionRef f =
    do let modFun session =
                        session { sess_data = f (sess_data session) }
       modifySessionBase vK sessionRef modFun

sessionMiddleware :: SessionCfg sess
                  -> V.Key SessionId
                  -> UserSessions conn sess st
                  -> Wai.Middleware
sessionMiddleware cfg vK sessionRef app req =
    case getCookieFromReq (sc_cookieName cfg) req of
      Just sid ->
          do mSess <- loadSessionImpl cfg sessionRef sid
             case mSess of
               Nothing ->
                   mkNew
               Just sess ->
                   withSess False sess
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
      withSess shouldSetCookie sess =
          do resp <- app (req { Wai.vault = V.insert vK (sess_id sess) v })
             return $ if shouldSetCookie then Wai.mapHeaders (addCookie sess) resp else resp
      mkNew =
          do newSess <- newSessionImpl cfg sessionRef defVal
             withSess True newSess

newSessionImpl :: SessionCfg sess
               -> UserSessions conn sess st
               -> sess
               -> IO (Session conn sess st)
newSessionImpl sessCfg sessionRef content =
    do sess <- createSession sessCfg content
       atomically $ modifyTVar sessionRef (\hm -> HM.insert (sess_id sess) sess hm)
       return sess

loadSessionImpl :: SessionCfg sess
                -> UserSessions conn sess st
                -> SessionId
                -> IO (Maybe (Session conn sess st))
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

deleteSessionImpl :: UserSessions conn sess st
                  -> SessionId
                  -> IO ()
deleteSessionImpl sessionRef sid =
    do atomically $ modifyTVar sessionRef (\hm -> HM.delete sid hm)
       return ()

createSession :: SessionCfg sess -> sess -> IO (Session conn sess st)
createSession sessCfg content =
    do sid <- randomHash (sc_sessionIdEntropy sessCfg)
       now <- getCurrentTime
       let validUntil = addUTCTime (sc_sessionTTL sessCfg) now
           emptySafeActions =
               SafeActionStore HM.empty HM.empty
       return (Session sid validUntil content emptySafeActions)

randomHash :: Int -> IO T.Text
randomHash len =
    do gen <- g
       return $ T.decodeUtf8 $ B64.encode $ BSC.pack $
              take len $ randoms gen
    where
      g = newStdGen :: IO StdGen
