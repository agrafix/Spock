{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts, DeriveGeneric, OverloadedStrings, DoAndIfThenElse, RankNTypes #-}
module Web.Spock.Internal.SessionManager
    ( createSessionManager
    , SessionId, Session(..), SessionManager(..)
    )
where

import Web.Spock.Internal.Types
import Web.Spock.Internal.CoreAction
import Web.Spock.Internal.Util

import Control.Arrow (first)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Data.List (foldl')
import Data.Time
#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif
import System.Random
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Vault.Lazy as V
import qualified Network.Wai as Wai

createSessionManager :: SessionCfg sess -> IO (SessionManager conn sess st)
createSessionManager cfg =
    do oldSess <- loadSessions
       cacheHM <- atomically $ newTVar oldSess
       vaultKey <- V.newKey
       _ <- forkIO (forever (housekeepSessions cacheHM storeSessions))
       return $ SessionManager
                  { sm_getSessionId = getSessionIdImpl vaultKey cacheHM
                  , sm_readSession = readSessionImpl vaultKey cacheHM
                  , sm_writeSession = writeSessionImpl vaultKey cacheHM
                  , sm_modifySession = modifySessionImpl vaultKey cacheHM
                  , sm_clearAllSessions = clearAllSessionsImpl cacheHM
                  , sm_middleware = sessionMiddleware cfg vaultKey cacheHM
                  , sm_addSafeAction = addSafeActionImpl vaultKey cacheHM
                  , sm_lookupSafeAction = lookupSafeActionImpl vaultKey cacheHM
                  , sm_removeSafeAction = removeSafeActionImpl vaultKey cacheHM
                  }
    where
      (loadSessions, storeSessions) =
          case sc_persistCfg cfg of
            Nothing ->
                ( return HM.empty
                , const $ return ()
                )
            Just spc ->
                ( do sessions <- spc_load spc
                     return $ foldl' genSession HM.empty sessions
                , \hm ->
                    spc_store spc $ map mkSerializable $ HM.elems hm
                )
      mkSerializable sess =
          (sess_id sess, sess_validUntil sess, sess_data sess)
      genSession hm (sid, validUntil, theData) =
          let s =
                  Session
                  { sess_id = sid
                  , sess_validUntil = validUntil
                  , sess_data = theData
                  , sess_safeActions = SafeActionStore HM.empty HM.empty
                  }
          in HM.insert sid s hm

getSessionIdImpl :: V.Key SessionId
                 -> UserSessions conn sess st
                 -> SpockAction conn sess st SessionId
getSessionIdImpl vK sessionRef =
    do sess <- readSessionBase vK sessionRef
       return $ sess_id sess

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
             liftIO $ atomically $ modifyTVar' sessionRef (HM.adjust modFun sid)

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
addSafeActionImpl vaultKey sessionMapVar safeAction =
    do base <- readSessionBase vaultKey sessionMapVar
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
                modifySessionBase vaultKey sessionMapVar (\s -> s { sess_safeActions = f (sess_safeActions s) })
                return safeActionHash

lookupSafeActionImpl :: V.Key SessionId
                     -> UserSessions conn sess st
                     -> SafeActionHash
                     -> SpockAction conn sess st (Maybe (PackedSafeAction conn sess st))
lookupSafeActionImpl vaultKey sessionMapVar hash =
    do base <- readSessionBase vaultKey sessionMapVar
       return $ HM.lookup hash (sas_forward (sess_safeActions base))

removeSafeActionImpl :: V.Key SessionId
                     -> UserSessions conn sess st
                     -> PackedSafeAction conn sess st
                     -> SpockAction conn sess st ()
removeSafeActionImpl vaultKey sessionMapVar action =
    modifySessionBase vaultKey sessionMapVar (\s -> s { sess_safeActions = f (sess_safeActions s ) })
    where
      f sas =
          sas
          { sas_forward =
              case HM.lookup action (sas_reverse sas) of
                Just h -> HM.delete h (sas_forward sas)
                Nothing -> sas_forward sas
          , sas_reverse = HM.delete action (sas_reverse sas)
          }

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
sessionMiddleware cfg vK sessionRef app req respond =
    case getCookieFromReq (sc_cookieName cfg) of
      Just sid ->
          do mSess <- loadSessionImpl sessionRef sid
             case mSess of
               Nothing ->
                   mkNew
               Just sess ->
                   withSess False sess
      Nothing ->
          mkNew
    where
      getCookieFromReq name =
          lookup "cookie" (Wai.requestHeaders req) >>=
                 lookup name . parseCookies . T.decodeUtf8
      renderCookie name value validUntil =
          let formattedTime =
                  TL.pack $ formatTime defaultTimeLocale "%a, %d-%b-%Y %X %Z" validUntil
          in TL.concat [ TL.fromStrict name
                       , "="
                       , TL.fromStrict value
                       , "; path=/; expires="
                       , formattedTime
                       , ";"
                       ]
      parseCookies :: T.Text -> [(T.Text, T.Text)]
      parseCookies = map parseCookie . T.splitOn ";" . T.concat . T.words
      parseCookie = first T.init . T.breakOnEnd "="

      defVal = sc_emptySession cfg
      v = Wai.vault req
      addCookie sess responseHeaders =
          let cookieContent =
                  renderCookie (sc_cookieName cfg) (sess_id sess) (sess_validUntil sess)
              cookieC = ("Set-Cookie", BSL.toStrict $ TL.encodeUtf8 cookieContent)
          in (cookieC : responseHeaders)
      withSess shouldSetCookie sess =
          app (req { Wai.vault = V.insert vK (sess_id sess) v }) $ \unwrappedResp ->
              respond $
              if shouldSetCookie
              then mapReqHeaders (addCookie sess) unwrappedResp
              else unwrappedResp
      mkNew =
          do newSess <- newSessionImpl cfg sessionRef defVal
             withSess True newSess

newSessionImpl :: SessionCfg sess
               -> UserSessions conn sess st
               -> sess
               -> IO (Session conn sess st)
newSessionImpl sessCfg sessionRef content =
    do sess <- createSession sessCfg content
       atomically $ modifyTVar' sessionRef (\hm -> HM.insert (sess_id sess) sess hm)
       return $! sess

loadSessionImpl :: UserSessions conn sess st
                -> SessionId
                -> IO (Maybe (Session conn sess st))
loadSessionImpl sessionRef sid =
    do sessHM <- atomically $ readTVar sessionRef
       now <- getCurrentTime
       case HM.lookup sid sessHM of
         Just sess ->
             do if (sess_validUntil sess) > now
                then return $ Just sess
                else do deleteSessionImpl sessionRef sid
                        return Nothing
         Nothing ->
             return Nothing

deleteSessionImpl :: UserSessions conn sess st
                  -> SessionId
                  -> IO ()
deleteSessionImpl sessionRef sid =
    do atomically $ modifyTVar' sessionRef (\hm -> HM.delete sid hm)
       return ()

clearAllSessionsImpl :: UserSessions conn sess st
                     -> SpockAction conn sess st ()
clearAllSessionsImpl sessionRef =
    liftIO $ atomically $ modifyTVar' sessionRef (const HM.empty)

housekeepSessions :: UserSessions conn sess st
                  -> (HM.HashMap SessionId (Session conn sess st) -> IO ())
                  -> IO ()
housekeepSessions sessionRef storeSessions =
    do now <- getCurrentTime
       newStatus <-
           atomically $
           do modifyTVar' sessionRef (killOld now)
              readTVar sessionRef
       storeSessions newStatus
       threadDelay (1000 * 1000 * 60) -- 60 seconds
    where
      filterOld now (_, sess) =
          (sess_validUntil sess) > now
      killOld now hm =
          HM.fromList $ filter (filterOld now) $ HM.toList hm

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
       return $ T.replace "=" "" $ T.decodeUtf8 $ B64.encode $ BSC.pack $
              take len $ randoms gen
    where
      g = newStdGen :: IO StdGen
