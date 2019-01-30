{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, DoAndIfThenElse, RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Web.Spock.Internal.SessionManager
    ( createSessionManager, withSessionManager
    , SessionId, Session(..), SessionManager(..)
    , SessionIf(..)
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
import qualified Crypto.Random as CR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.HashMap.Strict as HM
import qualified Data.Traversable as T
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vault.Lazy as V
import qualified Network.Wai as Wai

data SessionIf m
    = SessionIf
    { si_queryVault :: forall a. V.Key a -> m (Maybe a)
    , si_modifyVault :: (V.Vault -> V.Vault) -> m ()
    , si_setRawMultiHeader :: MultiHeader -> BS.ByteString -> m ()
    , si_vaultKey :: IO (V.Key SessionId)
    }

withSessionManager ::
    MonadIO m => SessionCfg conn sess st -> SessionIf m -> (SessionManager m conn sess st -> IO a) -> IO a
withSessionManager sessCfg sif =
    bracket (createSessionManager sessCfg sif) sm_closeSessionManager

createSessionManager ::
    MonadIO m => SessionCfg conn sess st -> SessionIf m -> IO (SessionManager m conn sess st)
createSessionManager cfg sif =
    do vaultKey <- si_vaultKey sif
       housekeepThread <- forkIO (forever (housekeepSessions cfg))
       return
          SessionManager
          { sm_getSessionId = getSessionIdImpl vaultKey cfg sif
          , sm_getCsrfToken = getCsrfTokenImpl vaultKey cfg sif
          , sm_regenerateSessionId = regenerateSessionIdImpl vaultKey store cfg sif
          , sm_readSession = readSessionImpl vaultKey cfg sif
          , sm_writeSession = writeSessionImpl vaultKey store cfg sif
          , sm_modifySession = modifySessionImpl vaultKey store cfg sif
          , sm_clearAllSessions = clearAllSessionsImpl store
          , sm_mapSessions = mapAllSessionsImpl store
          , sm_middleware = sessionMiddleware cfg vaultKey
          , sm_closeSessionManager = killThread housekeepThread
          }
    where
      store = sc_store cfg

regenerateSessionIdImpl ::
    MonadIO m
    => V.Key SessionId
    -> SessionStoreInstance (Session conn sess st)
    -> SessionCfg conn sess st
    -> SessionIf m
    -> m ()
regenerateSessionIdImpl vK sessionRef cfg sif =
    do sess <- readSessionBase vK cfg sif
       liftIO $ deleteSessionImpl sessionRef (sess_id sess)
       newSession <- liftIO $ newSessionImpl cfg sessionRef (sess_data sess)
       now <- liftIO getCurrentTime
       si_setRawMultiHeader sif MultiHeaderSetCookie (makeSessionIdCookie cfg newSession now)
       si_modifyVault sif $ V.insert vK (sess_id newSession)

getSessionIdImpl ::
    MonadIO m
    => V.Key SessionId
    -> SessionCfg conn sess st
    -> SessionIf m
    -> m SessionId
getSessionIdImpl vK cfg sif =
    do sess <- readSessionBase vK cfg sif
       return $ sess_id sess

getCsrfTokenImpl ::
    ( MonadIO m )
    => V.Key SessionId
    -> SessionCfg conn sess st
    -> SessionIf m
    -> m T.Text
getCsrfTokenImpl vK cfg sif =
    do sess <- readSessionBase vK cfg sif
       return $ sess_csrfToken sess

modifySessionBase ::
    MonadIO m
    => V.Key SessionId
    -> SessionStoreInstance (Session conn sess st)
    -> SessionCfg conn sess st
    -> SessionIf m
    -> (Session conn sess st -> (Session conn sess st, a))
    -> m a
modifySessionBase vK (SessionStoreInstance sessionRef) cfg sif modFun =
    do mValue <- si_queryVault sif vK
       case mValue of
         Nothing ->
             error "(3) Internal Spock Session Error. Please report this bug!"
         Just sid ->
             do session <- readOrNewSession cfg vK sif (Just sid)
                let (sessionNew, result) = modFun session
                liftIO $ ss_runTx sessionRef $ ss_storeSession sessionRef sessionNew
                return result

readSessionBase ::
    MonadIO m
    => V.Key SessionId
    -> SessionCfg conn sess st
    -> SessionIf m
    -> m (Session conn sess st)
readSessionBase vK cfg sif =
    do mValue <- si_queryVault sif vK
       case mValue of
         Nothing ->
             error "(1) Internal Spock Session Error. Please report this bug!"
         Just sid ->
             readOrNewSession cfg vK sif (Just sid)

readSessionImpl ::
    MonadIO m
    => V.Key SessionId
    -> SessionCfg conn sess st
    -> SessionIf m
    -> m sess
readSessionImpl vK cfg sif =
    do base <- readSessionBase vK cfg sif
       return (sess_data base)

writeSessionImpl ::
    MonadIO m
    => V.Key SessionId
    -> SessionStoreInstance (Session conn sess st)
    -> SessionCfg conn sess st
    -> SessionIf m
    -> sess
    -> m ()
writeSessionImpl vK sessionRef cfg sif value =
    modifySessionImpl vK sessionRef cfg sif (const (value, ()))

modifySessionImpl ::
    MonadIO m
    => V.Key SessionId
    -> SessionStoreInstance (Session conn sess st)
    -> SessionCfg conn sess st
    -> SessionIf m
    -> (sess -> (sess, a))
    -> m a
modifySessionImpl vK sessionRef cfg sif f =
    do let modFun session =
               let (sessData', out) = f (sess_data session)
               in (session { sess_data = sessData' }, out)
       modifySessionBase vK sessionRef cfg sif modFun

makeSessionIdCookie :: SessionCfg conn sess st -> Session conn sess st -> UTCTime -> BS.ByteString
makeSessionIdCookie cfg sess now =
    generateCookieHeaderString name value settings now
    where
      name = sc_cookieName cfg
      value = sess_id sess
      settings =
          defaultCookieSettings
          { cs_EOL = sc_cookieEOL cfg
          , cs_HTTPOnly = True
          }

readOrNewSession ::
    MonadIO m
    => SessionCfg conn sess st
    -> V.Key SessionId
    -> SessionIf m
    -> Maybe SessionId
    -> m (Session conn sess st)
readOrNewSession cfg vK sif mSid =
    do (sess, write) <- loadOrSpanSession cfg mSid
       when write $
           do now <- liftIO getCurrentTime
              si_setRawMultiHeader sif MultiHeaderSetCookie (makeSessionIdCookie cfg sess now)
              si_modifyVault sif $ V.insert vK (sess_id sess)
       return sess

loadOrSpanSession ::
    MonadIO m
    => SessionCfg conn sess st
    -> Maybe SessionId
    -> m (Session conn sess st, Bool)
loadOrSpanSession cfg mSid =
    do mSess <-
           liftIO $
           join <$> T.mapM (loadSessionImpl cfg sessionRef) mSid
       case mSess of
         Nothing ->
             do newSess <-
                    liftIO $
                    newSessionImpl cfg sessionRef (sc_emptySession cfg)
                return (newSess, True)
         Just s -> return (s, False)
    where
        sessionRef = sc_store cfg

sessionMiddleware ::
    SessionCfg conn sess st
    -> V.Key SessionId
    -> Wai.Middleware
sessionMiddleware cfg vK app req respond =
    go $ getCookieFromReq (sc_cookieName cfg)
    where
      go mSid =
          do (sess, shouldWriteCookie) <- loadOrSpanSession cfg mSid
             withSess shouldWriteCookie sess
      getCookieFromReq name =
          lookup "cookie" (Wai.requestHeaders req) >>= lookup name . parseCookies
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

newSessionImpl ::
    SessionCfg conn sess st
    -> SessionStoreInstance (Session conn sess st)
    -> sess
    -> IO (Session conn sess st)
newSessionImpl sessCfg (SessionStoreInstance sessionRef) content =
    do sess <- createSession sessCfg content
       ss_runTx sessionRef $ ss_storeSession sessionRef sess
       return $! sess

loadSessionImpl ::
    SessionCfg conn sess st
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

deleteSessionImpl ::
    SessionStoreInstance (Session conn sess st)
    -> SessionId
    -> IO ()
deleteSessionImpl (SessionStoreInstance sessionRef) sid =
    ss_runTx sessionRef $ ss_deleteSession sessionRef sid

clearAllSessionsImpl ::
    MonadIO m
    => SessionStoreInstance (Session conn sess st)
    -> m ()
clearAllSessionsImpl (SessionStoreInstance sessionRef) =
    liftIO $ ss_runTx sessionRef $ ss_filterSessions sessionRef (const False)

mapAllSessionsImpl ::
    MonadIO m
    => SessionStoreInstance (Session conn sess st)
    -> (forall n. Monad n => sess -> n sess)
    -> m ()
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

createSession :: SessionCfg conn sess st -> sess -> IO (Session conn sess st)
createSession sessCfg content =
    do sid <- randomHash (sc_sessionIdEntropy sessCfg)
       csrfToken <- randomHash 12
       now <- getCurrentTime
       let validUntil = addUTCTime (sc_sessionTTL sessCfg) now
       return (Session sid csrfToken validUntil content)

randomHash :: Int -> IO T.Text
randomHash len =
    do by <- CR.getRandomBytes len
       return $ T.replace "=" "" $ T.replace "/" "_" $ T.replace "+" "-" $
              T.decodeUtf8 $ B64.encode by
