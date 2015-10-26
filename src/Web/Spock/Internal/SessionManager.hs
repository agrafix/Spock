{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, DoAndIfThenElse, RankNTypes #-}
module Web.Spock.Internal.SessionManager
    ( createSessionManager, withSessionManager
    , SessionId, Session(..), SessionManager(..)
    )
where

import Web.Spock.Internal.Types
import Web.Spock.Internal.CoreAction
import Web.Spock.Internal.Wire
import Web.Spock.Internal.Util
import Web.Spock.Internal.Cookies
import qualified Web.Spock.Internal.SessionVault as SV

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.Time
#if MIN_VERSION_time(1,5,0)
#else
import System.Locale (defaultTimeLocale)
#endif
import qualified Crypto.Random as CR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vault.Lazy as V
import qualified Network.Wai as Wai

withSessionManager :: SessionCfg sess -> (SessionManager conn sess st -> IO a) -> IO a
withSessionManager sessCfg =
    bracket (createSessionManager sessCfg) sm_closeSessionManager

createSessionManager :: SessionCfg sess -> IO (SessionManager conn sess st)
createSessionManager cfg =
    do pool <- CR.createEntropyPool
       oldSess <- loadSessions
       cacheHM <-
           atomically $
           do mapV <- SV.newSessionVault
              forM_ oldSess $ \v -> SV.storeSession v mapV
              return mapV
       vaultKey <- V.newKey
       housekeepThread <- forkIO (forever (housekeepSessions cfg cacheHM storeSessions))
       return
          SessionManager
          { sm_getSessionId = getSessionIdImpl vaultKey cacheHM
          , sm_regenerateSessionId = regenerateSessionIdImpl vaultKey cacheHM pool cfg
          , sm_readSession = readSessionImpl vaultKey cacheHM
          , sm_writeSession = writeSessionImpl vaultKey cacheHM
          , sm_modifySession = modifySessionImpl vaultKey cacheHM
          , sm_clearAllSessions = clearAllSessionsImpl cacheHM
          , sm_mapSessions = mapAllSessionsImpl cacheHM
          , sm_middleware = sessionMiddleware pool cfg vaultKey cacheHM
          , sm_addSafeAction = addSafeActionImpl pool vaultKey cacheHM
          , sm_lookupSafeAction = lookupSafeActionImpl vaultKey cacheHM
          , sm_removeSafeAction = removeSafeActionImpl vaultKey cacheHM
          , sm_closeSessionManager = killThread housekeepThread
          }
    where
      (loadSessions, storeSessions) =
          case sc_persistCfg cfg of
            Nothing ->
                ( return []
                , const $ return ()
                )
            Just spc ->
                ( do sessions <- spc_load spc
                     return (map genSession sessions)
                , spc_store spc . map mkSerializable . HM.elems
                )
      mkSerializable sess =
          (sess_id sess, sess_validUntil sess, sess_data sess)
      genSession (sid, validUntil, theData) =
          Session
          { sess_id = sid
          , sess_validUntil = validUntil
          , sess_data = theData
          , sess_safeActions = SafeActionStore HM.empty HM.empty
          }

regenerateSessionIdImpl ::
    V.Key SessionId
    -> SV.SessionVault (Session conn sess st)
    -> CR.EntropyPool
    -> SessionCfg sess
    -> SpockActionCtx ctx conn sess st ()
regenerateSessionIdImpl vK sessionRef entropyPool cfg =
    do sess <- readSessionBase vK sessionRef
       liftIO $ deleteSessionImpl sessionRef (sess_id sess)
       newSession <- liftIO $ newSessionImpl entropyPool cfg sessionRef (sess_data sess)
       now <- liftIO getCurrentTime
       setRawMultiHeader MultiHeaderSetCookie $ makeSessionIdCookie cfg newSession now
       modifyVault $ V.insert vK (sess_id newSession)

getSessionIdImpl :: V.Key SessionId
                 -> SV.SessionVault (Session conn sess st)
                 -> SpockActionCtx ctx conn sess st SessionId
getSessionIdImpl vK sessionRef =
    do sess <- readSessionBase vK sessionRef
       return $ sess_id sess

modifySessionBase :: V.Key SessionId
                  -> SV.SessionVault (Session conn sess st)
                  -> (Session conn sess st -> (Session conn sess st, a))
                  -> SpockActionCtx ctx conn sess st a
modifySessionBase vK sessionRef modFun =
    do mValue <- queryVault vK
       case mValue of
         Nothing ->
             error "(3) Internal Spock Session Error. Please report this bug!"
         Just sid ->
             liftIO $ atomically $
             do mSession <- SV.loadSession sid sessionRef
                case mSession of
                  Nothing ->
                      fail "Internal Spock Session Error: Unknown SessionId"
                  Just session ->
                      do let (sessionNew, result) = modFun session
                         SV.storeSession sessionNew sessionRef
                         return result

readSessionBase :: V.Key SessionId
                -> SV.SessionVault (Session conn sess st)
                -> SpockActionCtx ctx conn sess st (Session conn sess st)
readSessionBase vK sessionRef =
    do mValue <- queryVault vK
       case mValue of
         Nothing ->
             error "(1) Internal Spock Session Error. Please report this bug!"
         Just sid ->
             do mSession <- liftIO $ atomically $ SV.loadSession sid sessionRef
                case mSession of
                  Nothing ->
                      error "(2) Internal Spock Session Error. Please report this bug!"
                  Just session ->
                      return session

addSafeActionImpl ::
    CR.EntropyPool
    -> V.Key SessionId
    -> SV.SessionVault (Session conn sess st)
    -> PackedSafeAction conn sess st
    -> SpockActionCtx ctx conn sess st SafeActionHash
addSafeActionImpl pool vaultKey sessionMapVar safeAction =
    do base <- readSessionBase vaultKey sessionMapVar
       case HM.lookup safeAction (sas_reverse (sess_safeActions base)) of
         Just safeActionHash ->
             return safeActionHash
         Nothing ->
             do safeActionHash <- liftIO (randomHash pool 40)
                let f sas =
                        sas
                        { sas_forward = HM.insert safeActionHash safeAction (sas_forward sas)
                        , sas_reverse = HM.insert safeAction safeActionHash (sas_reverse sas)
                        }
                modifySessionBase vaultKey sessionMapVar (\s -> (s { sess_safeActions = f (sess_safeActions s) }, ()))
                return safeActionHash

lookupSafeActionImpl :: V.Key SessionId
                     -> SV.SessionVault (Session conn sess st)
                     -> SafeActionHash
                     -> SpockActionCtx ctx conn sess st (Maybe (PackedSafeAction conn sess st))
lookupSafeActionImpl vaultKey sessionMapVar hash =
    do base <- readSessionBase vaultKey sessionMapVar
       return $ HM.lookup hash (sas_forward (sess_safeActions base))

removeSafeActionImpl ::
    V.Key SessionId
    -> SV.SessionVault (Session conn sess st)
    -> PackedSafeAction conn sess st
    -> SpockActionCtx ctx conn sess st ()
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
                -> SV.SessionVault (Session conn sess st)
                -> SpockActionCtx ctx conn sess st sess
readSessionImpl vK sessionRef =
    do base <- readSessionBase vK sessionRef
       return (sess_data base)

writeSessionImpl :: V.Key SessionId
                 -> SV.SessionVault (Session conn sess st)
                 -> sess
                 -> SpockActionCtx ctx conn sess st ()
writeSessionImpl vK sessionRef value =
    modifySessionImpl vK sessionRef (const (value, ()))

modifySessionImpl :: V.Key SessionId
                  -> SV.SessionVault (Session conn sess st)
                  -> (sess -> (sess, a))
                  -> SpockActionCtx ctx conn sess st a
modifySessionImpl vK sessionRef f =
    do let modFun session =
               let (sessData', out) = f (sess_data session)
               in (session { sess_data = sessData' }, out)
       modifySessionBase vK sessionRef modFun

makeSessionIdCookie :: SessionCfg sess -> Session conn sess st -> UTCTime -> BS.ByteString
makeSessionIdCookie cfg sess now =
    generateCookieHeaderString name value settings now
    where
      name = sc_cookieName cfg
      value = sess_id sess
      settings =
          defaultCookieSettings
          { cs_EOL = CookieValidUntil (sess_validUntil sess)
          , cs_HTTPOnly = True
          }

sessionMiddleware ::
    CR.EntropyPool
    -> SessionCfg sess
    -> V.Key SessionId
    -> SV.SessionVault (Session conn sess st)
    -> Wai.Middleware
sessionMiddleware pool cfg vK sessionRef app req respond =
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
                 lookup name . parseCookies
      defVal = sc_emptySession cfg
      v = Wai.vault req
      addCookie sess now responseHeaders =
          let cookieContent = makeSessionIdCookie cfg sess now
              cookieC = ("Set-Cookie", cookieContent)
          in (cookieC : responseHeaders)
      withSess shouldSetCookie sess =
          app (req { Wai.vault = V.insert vK (sess_id sess) v }) $ \unwrappedResp ->
              do now <- getCurrentTime
                 respond $
                   if shouldSetCookie
                   then mapReqHeaders (addCookie sess now) unwrappedResp
                   else unwrappedResp
      mkNew =
          do newSess <- newSessionImpl pool cfg sessionRef defVal
             withSess True newSess

newSessionImpl ::
    CR.EntropyPool
    -> SessionCfg sess
    -> SV.SessionVault (Session conn sess st)
    -> sess
    -> IO (Session conn sess st)
newSessionImpl pool sessCfg sessionRef content =
    do sess <- createSession pool sessCfg content
       atomically $ SV.storeSession sess sessionRef
       return $! sess

loadSessionImpl :: SessionCfg sess
                -> SV.SessionVault (Session conn sess st)
                -> SessionId
                -> IO (Maybe (Session conn sess st))
loadSessionImpl sessCfg sessionRef sid =
    do mSess <- atomically $ SV.loadSession sid sessionRef
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
                            atomically $ SV.storeSession expandedSession sessionRef
                            return expandedSession
                    else return sess
                if sess_validUntil sessWithPossibleExpansion > now
                then return $ Just sessWithPossibleExpansion
                else do deleteSessionImpl sessionRef sid
                        return Nothing
         Nothing ->
             return Nothing

deleteSessionImpl :: SV.SessionVault (Session conn sess st)
                  -> SessionId
                  -> IO ()
deleteSessionImpl sessionRef sid =
    atomically $ SV.deleteSession sid sessionRef

clearAllSessionsImpl :: SV.SessionVault (Session conn sess st)
                     -> SpockActionCtx ctx conn sess st ()
clearAllSessionsImpl sessionRef =
    liftIO $ atomically $ SV.filterSessions (const False) sessionRef

mapAllSessionsImpl ::
    SV.SessionVault (Session conn sess st)
    -> (sess -> STM sess)
    -> SpockActionCtx ctx conn sess st ()
mapAllSessionsImpl sessionRef f =
    liftIO $ atomically $ flip SV.mapSessions sessionRef $ \sess ->
        do newData <- f (sess_data sess)
           return $ sess { sess_data = newData }

housekeepSessions :: SessionCfg sess
                  -> SV.SessionVault (Session conn sess st)
                  -> (HM.HashMap SessionId (Session conn sess st) -> IO ())
                  -> IO ()
housekeepSessions cfg sessionRef storeSessions =
    do now <- getCurrentTime
       (newStatus, oldStatus) <-
           atomically $
           do oldSt <- SV.toList sessionRef
              SV.filterSessions (\sess -> sess_validUntil sess > now) sessionRef
              (,) <$> SV.toList sessionRef <*> pure oldSt
       let packSessionHm = HM.fromList . map (\v -> (SV.getSessionKey v, v))
           oldHm = packSessionHm oldStatus
           newHm = packSessionHm newStatus
       storeSessions newHm
       sh_removed (sc_hooks cfg) (HM.map sess_data $ oldHm `HM.difference` newHm)
       threadDelay (1000 * 1000 * (round $ sc_housekeepingInterval cfg))

createSession :: CR.EntropyPool -> SessionCfg sess -> sess -> IO (Session conn sess st)
createSession pool sessCfg content =
    do sid <- randomHash pool (sc_sessionIdEntropy sessCfg)
       now <- getCurrentTime
       let validUntil = addUTCTime (sc_sessionTTL sessCfg) now
           emptySafeActions =
               SafeActionStore HM.empty HM.empty
       return (Session sid validUntil content emptySafeActions)

randomHash :: CR.EntropyPool -> Int -> IO T.Text
randomHash pool len =
    do let sys :: CR.SystemRNG
           sys = CR.cprgCreate pool
       return $ T.replace "=" "" $ T.replace "/" "_" $ T.replace "+" "-" $
              T.decodeUtf8 $ B64.encode $ fst $ CR.cprgGenerateWithEntropy len sys
