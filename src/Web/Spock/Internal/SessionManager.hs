{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, DoAndIfThenElse, RankNTypes #-}
module Web.Spock.Internal.SessionManager
    ( createSessionManager, withSessionManager
    , SessionId, Session(..), SessionManager(..)
    )
where

import Web.Spock.Internal.Types
import Web.Spock.Internal.CoreAction
import Web.Spock.Internal.Util

import Control.Arrow (first)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.List (foldl')
import Data.Time
#if MIN_VERSION_time(1,5,0)
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
import qualified ListT as L
import qualified Network.Wai as Wai
import qualified STMContainers.Map as STMMap

withSessionManager :: SessionCfg sess -> (SessionManager conn sess st -> IO a) -> IO a
withSessionManager sessCfg =
    bracket (createSessionManager sessCfg) sm_closeSessionManager

createSessionManager :: SessionCfg sess -> IO (SessionManager conn sess st)
createSessionManager cfg =
    do oldSess <- loadSessions
       cacheHM <-
           atomically $
           do mapV <- STMMap.new
              forM_ (HM.toList oldSess) $ \(k, v) ->
                  STMMap.insert v k mapV
              return mapV
       vaultKey <- V.newKey
       housekeepThread <- forkIO (forever (housekeepSessions cacheHM storeSessions))
       return
          SessionManager
          { sm_getSessionId = getSessionIdImpl vaultKey cacheHM
          , sm_readSession = readSessionImpl vaultKey cacheHM
          , sm_writeSession = writeSessionImpl vaultKey cacheHM
          , sm_modifySession = modifySessionImpl vaultKey cacheHM
          , sm_clearAllSessions = clearAllSessionsImpl cacheHM
          , sm_middleware = sessionMiddleware cfg vaultKey cacheHM
          , sm_addSafeAction = addSafeActionImpl vaultKey cacheHM
          , sm_lookupSafeAction = lookupSafeActionImpl vaultKey cacheHM
          , sm_removeSafeAction = removeSafeActionImpl vaultKey cacheHM
          , sm_closeSessionManager = killThread housekeepThread
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
                , spc_store spc . map mkSerializable . HM.elems
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
                  -> (Session conn sess st -> (Session conn sess st, a))
                  -> SpockAction conn sess st a
modifySessionBase vK sessionRef modFun =
    do req <- request
       case V.lookup vK (Wai.vault req) of
         Nothing ->
             error "(3) Internal Spock Session Error. Please report this bug!"
         Just sid ->
             liftIO $ atomically $
             do mSession <- STMMap.lookup sid sessionRef
                case mSession of
                  Nothing ->
                      fail "Internal Spock Session Error: Unknown SessionId"
                  Just session ->
                      do let (sessionNew, result) = modFun session
                         STMMap.insert sessionNew sid sessionRef
                         return result

readSessionBase :: V.Key SessionId
                -> UserSessions conn sess st
                -> SpockAction conn sess st (Session conn sess st)
readSessionBase vK sessionRef =
    do req <- request
       case V.lookup vK (Wai.vault req) of
         Nothing ->
             error "(1) Internal Spock Session Error. Please report this bug!"
         Just sid ->
             do mSession <- liftIO $ atomically $ STMMap.lookup sid sessionRef
                case mSession of
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
                modifySessionBase vaultKey sessionMapVar (\s -> (s { sess_safeActions = f (sess_safeActions s) }, ()))
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
    modifySessionBase vaultKey sessionMapVar (\s -> (s { sess_safeActions = f (sess_safeActions s ) }, ()))
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
    modifySessionImpl vK sessionRef (const (value, ()))

modifySessionImpl :: V.Key SessionId
                  -> UserSessions conn sess st
                  -> (sess -> (sess, a))
                  -> SpockAction conn sess st a
modifySessionImpl vK sessionRef f =
    do let modFun session =
               let (sessData', out) = f (sess_data session)
               in (session { sess_data = sessData' }, out)
       modifySessionBase vK sessionRef modFun

sessionMiddleware :: SessionCfg sess
                  -> V.Key SessionId
                  -> UserSessions conn sess st
                  -> Wai.Middleware
sessionMiddleware cfg vK sessionRef app req respond =
    case getCookieFromReq (sc_cookieName cfg) of
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
       atomically $ STMMap.insert sess (sess_id sess) sessionRef
       return $! sess

loadSessionImpl :: SessionCfg sess
                -> UserSessions conn sess st
                -> SessionId
                -> IO (Maybe (Session conn sess st))
loadSessionImpl sessCfg sessionRef sid =
    do mSess <- atomically $ STMMap.lookup sid sessionRef
       now <- getCurrentTime
       case mSess of
         Just sess ->
             do sessWithPossibleExpansion <-
                    if sc_sessionExpandTTL sessCfg
                    then do let expandedSession =
                                    sess
                                    { sess_validUntil =
                                          addUTCTime (sc_sessionTTL sessCfg) now
                                    }
                            atomically $ STMMap.insert expandedSession sid sessionRef
                            return expandedSession
                    else return sess
                if sess_validUntil sessWithPossibleExpansion > now
                then return $ Just sessWithPossibleExpansion
                else do deleteSessionImpl sessionRef sid
                        return Nothing
         Nothing ->
             return Nothing

deleteSessionImpl :: UserSessions conn sess st
                  -> SessionId
                  -> IO ()
deleteSessionImpl sessionRef sid =
    atomically $ STMMap.delete sid sessionRef

clearAllSessionsImpl :: UserSessions conn sess st
                     -> SpockAction conn sess st ()
clearAllSessionsImpl sessionRef =
    liftIO $ atomically $
    do keys <- liftM (map fst) (getAllSessions sessionRef)
       forM_ keys $ \k -> STMMap.delete k sessionRef

getAllSessions :: UserSessions conn sess st -> STM [(SessionId, Session conn sess st)]
getAllSessions = L.toList . STMMap.stream

housekeepSessions :: UserSessions conn sess st
                  -> (HM.HashMap SessionId (Session conn sess st) -> IO ())
                  -> IO ()
housekeepSessions sessionRef storeSessions =
    do now <- getCurrentTime
       newStatus <-
           atomically $
           do allSessions <- getAllSessions sessionRef
              let deletableSessions =
                      map fst $
                      filter (filterOld now) allSessions
              forM_ deletableSessions $ \k -> STMMap.delete k sessionRef
              return $ filter (not . filterOld now) allSessions
       storeSessions (HM.fromList newStatus)
       threadDelay (1000 * 1000 * 60) -- 60 seconds
    where
      filterOld now (_, sess) = sess_validUntil sess <= now

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
