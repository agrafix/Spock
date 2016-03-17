{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, DoAndIfThenElse, RankNTypes #-}
module Web.Spock.Internal.SessionManager
    ( createSessionManager, withSessionManager
    , SessionId, Session(..), SessionManager(..)
    )
where

import Web.Spock.Core
import Web.Spock.Internal.Types
import Web.Spock.Internal.Util
import Web.Spock.Internal.Cookies

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif
import Control.Concurrent
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

withSessionManager :: SessionCfg conn sess st -> (SessionManager conn sess st -> IO a) -> IO a
withSessionManager sessCfg =
    bracket (createSessionManager sessCfg) sm_closeSessionManager

createSessionManager :: SessionCfg conn sess st -> IO (SessionManager conn sess st)
createSessionManager cfg =
    do pool <- CR.createEntropyPool
       vaultKey <- V.newKey
       housekeepThread <- forkIO (forever (housekeepSessions cfg))
       return
          SessionManager
          { sm_getSessionId = getSessionIdImpl vaultKey store
          , sm_regenerateSessionId = regenerateSessionIdImpl vaultKey store pool cfg
          , sm_readSession = readSessionImpl vaultKey store
          , sm_writeSession = writeSessionImpl vaultKey store
          , sm_modifySession = modifySessionImpl vaultKey store
          , sm_clearAllSessions = clearAllSessionsImpl store
          , sm_mapSessions = mapAllSessionsImpl store
          , sm_middleware = sessionMiddleware pool cfg vaultKey
          , sm_addSafeAction = addSafeActionImpl pool vaultKey store
          , sm_lookupSafeAction = lookupSafeActionImpl vaultKey store
          , sm_removeSafeAction = removeSafeActionImpl vaultKey store
          , sm_closeSessionManager = killThread housekeepThread
          }
    where
      store = sc_store cfg

regenerateSessionIdImpl ::
    V.Key SessionId
    -> SessionStoreInstance (Session conn sess st)
    -> CR.EntropyPool
    -> SessionCfg conn sess st
    -> SpockActionCtx ctx conn sess st ()
regenerateSessionIdImpl vK sessionRef entropyPool cfg =
    do sess <- readSessionBase vK sessionRef
       liftIO $ deleteSessionImpl sessionRef (sess_id sess)
       newSession <- liftIO $ newSessionImpl entropyPool cfg sessionRef (sess_data sess)
       now <- liftIO getCurrentTime
       setRawMultiHeader MultiHeaderSetCookie $ makeSessionIdCookie cfg newSession now
       modifyVault $ V.insert vK (sess_id newSession)

getSessionIdImpl :: V.Key SessionId
                 -> SessionStoreInstance (Session conn sess st)
                 -> SpockActionCtx ctx conn sess st SessionId
getSessionIdImpl vK sessionRef =
    do sess <- readSessionBase vK sessionRef
       return $ sess_id sess

modifySessionBase :: V.Key SessionId
                  -> SessionStoreInstance (Session conn sess st)
                  -> (Session conn sess st -> (Session conn sess st, a))
                  -> SpockActionCtx ctx conn sess st a
modifySessionBase vK (SessionStoreInstance sessionRef) modFun =
    do mValue <- queryVault vK
       case mValue of
         Nothing ->
             error "(3) Internal Spock Session Error. Please report this bug!"
         Just sid ->
             liftIO $ ss_runTx sessionRef $
             do mSession <- ss_loadSession sessionRef sid
                case mSession of
                  Nothing ->
                      fail "Internal Spock Session Error: Unknown SessionId"
                  Just session ->
                      do let (sessionNew, result) = modFun session
                         ss_storeSession sessionRef sessionNew
                         return result

readSessionBase :: V.Key SessionId
                -> SessionStoreInstance (Session conn sess st)
                -> SpockActionCtx ctx conn sess st (Session conn sess st)
readSessionBase vK (SessionStoreInstance sessionRef) =
    do mValue <- queryVault vK
       case mValue of
         Nothing ->
             error "(1) Internal Spock Session Error. Please report this bug!"
         Just sid ->
             do mSession <- liftIO $ ss_runTx sessionRef $ ss_loadSession sessionRef sid
                case mSession of
                  Nothing ->
                      error "(2) Internal Spock Session Error. Please report this bug!"
                  Just session ->
                      return session

addSafeActionImpl ::
    CR.EntropyPool
    -> V.Key SessionId
    -> SessionStoreInstance (Session conn sess st)
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
                     -> SessionStoreInstance (Session conn sess st)
                     -> SafeActionHash
                     -> SpockActionCtx ctx conn sess st (Maybe (PackedSafeAction conn sess st))
lookupSafeActionImpl vaultKey sessionMapVar hash =
    do base <- readSessionBase vaultKey sessionMapVar
       return $ HM.lookup hash (sas_forward (sess_safeActions base))

removeSafeActionImpl ::
    V.Key SessionId
    -> SessionStoreInstance (Session conn sess st)
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
                -> SessionStoreInstance (Session conn sess st)
                -> SpockActionCtx ctx conn sess st sess
readSessionImpl vK sessionRef =
    do base <- readSessionBase vK sessionRef
       return (sess_data base)

writeSessionImpl :: V.Key SessionId
                 -> SessionStoreInstance (Session conn sess st)
                 -> sess
                 -> SpockActionCtx ctx conn sess st ()
writeSessionImpl vK sessionRef value =
    modifySessionImpl vK sessionRef (const (value, ()))

modifySessionImpl :: V.Key SessionId
                  -> SessionStoreInstance (Session conn sess st)
                  -> (sess -> (sess, a))
                  -> SpockActionCtx ctx conn sess st a
modifySessionImpl vK sessionRef f =
    do let modFun session =
               let (sessData', out) = f (sess_data session)
               in (session { sess_data = sessData' }, out)
       modifySessionBase vK sessionRef modFun

makeSessionIdCookie :: SessionCfg conn sess st -> Session conn sess st -> UTCTime -> BS.ByteString
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
    -> SessionCfg conn sess st
    -> V.Key SessionId
    -> Wai.Middleware
sessionMiddleware pool cfg vK app req respond =
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
      sessionRef = sc_store cfg

newSessionImpl ::
    CR.EntropyPool
    -> SessionCfg conn sess st
    -> SessionStoreInstance (Session conn sess st)
    -> sess
    -> IO (Session conn sess st)
newSessionImpl pool sessCfg (SessionStoreInstance sessionRef) content =
    do sess <- createSession pool sessCfg content
       ss_runTx sessionRef $ ss_storeSession sessionRef sess
       return $! sess

loadSessionImpl :: SessionCfg conn sess st
                -> SessionStoreInstance (Session conn sess st)
                -> SessionId
                -> IO (Maybe (Session conn sess st))
loadSessionImpl sessCfg sessionRef@(SessionStoreInstance store) sid =
    do mSess <- ss_runTx store $ ss_loadSession store sid
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
                            ss_runTx store $ ss_storeSession store expandedSession
                            return expandedSession
                    else return sess
                if sess_validUntil sessWithPossibleExpansion > now
                then return $ Just sessWithPossibleExpansion
                else do deleteSessionImpl sessionRef sid
                        return Nothing
         Nothing ->
             return Nothing

deleteSessionImpl :: SessionStoreInstance (Session conn sess st)
                  -> SessionId
                  -> IO ()
deleteSessionImpl (SessionStoreInstance sessionRef) sid =
    ss_runTx sessionRef $ ss_deleteSession sessionRef sid

clearAllSessionsImpl :: SessionStoreInstance (Session conn sess st)
                     -> SpockActionCtx ctx conn sess st ()
clearAllSessionsImpl (SessionStoreInstance sessionRef) =
    liftIO $ ss_runTx sessionRef $ ss_filterSessions sessionRef (const False)

mapAllSessionsImpl ::
    SessionStoreInstance (Session conn sess st)
    -> (forall m. Monad m => sess -> m sess)
    -> SpockActionCtx ctx conn sess st ()
mapAllSessionsImpl (SessionStoreInstance sessionRef) f =
    liftIO $ ss_runTx sessionRef $ ss_mapSessions sessionRef $ \sess ->
        do newData <- f (sess_data sess)
           return $ sess { sess_data = newData }

housekeepSessions :: SessionCfg conn sess st -> IO ()
housekeepSessions cfg =
    case sc_store cfg of
      SessionStoreInstance store ->
       do now <- getCurrentTime
          (newStatus, oldStatus) <-
            ss_runTx store $
            do oldSt <- ss_toList store
               ss_filterSessions store (\sess -> sess_validUntil sess > now)
               (,) <$> ss_toList store <*> pure oldSt
          let packSessionHm = HM.fromList . map (\v -> (sess_id v, v))
              oldHm = packSessionHm oldStatus
              newHm = packSessionHm newStatus
          sh_removed (sc_hooks cfg) (HM.map sess_data $ oldHm `HM.difference` newHm)
          threadDelay (1000 * 1000 * (round $ sc_housekeepingInterval cfg))

createSession :: CR.EntropyPool -> SessionCfg conn sess st -> sess -> IO (Session conn sess st)
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
