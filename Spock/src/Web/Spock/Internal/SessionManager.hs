{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Web.Spock.Internal.SessionManager
  ( createSessionManager,
    withSessionManager,
    SessionId,
    Session (..),
    SessionManager (..),
    SessionIf (..),
  )
where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import qualified Crypto.Random as CR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import qualified Data.Traversable as T
import qualified Data.Vault.Lazy as V
import qualified Network.Wai as Wai
import Web.Spock.Core
import Web.Spock.Internal.Cookies
import Web.Spock.Internal.Types
import Web.Spock.Internal.Util

data SessionIf m = SessionIf
  { si_queryVault :: forall a. V.Key a -> m (Maybe a),
    si_modifyVault :: (V.Vault -> V.Vault) -> m (),
    si_setRawMultiHeader :: MultiHeader -> BS.ByteString -> m (),
    si_vaultKey :: IO (V.Key SessionId)
  }

withSessionManager ::
  MonadIO m => SessionCfg conn sess st -> SessionIf m -> (SessionManager m conn sess st -> IO a) -> IO a
withSessionManager sessCfg sif =
  bracket (createSessionManager sessCfg sif) sm_closeSessionManager

createSessionManager ::
  MonadIO m => SessionCfg conn sess st -> SessionIf m -> IO (SessionManager m conn sess st)
createSessionManager cfg originalIf =
  do
    cookieKey <- V.newKey
    -- Share the pending cookie with the response middleware. The last session
    -- action wins, even on the first request or when a handler falls through.
    let sif = originalIf
          { si_setRawMultiHeader = \header value -> do
              pending <- si_queryVault originalIf cookieKey
              case pending of
                Nothing -> si_setRawMultiHeader originalIf header value
                Just ref -> liftIO $ writeIORef ref (Just value)
          }
    vaultKey <- si_vaultKey sif
    housekeepThread <-
      if sc_sessionMode cfg == SessionsDisabled
        then pure Nothing
        else Just <$> forkIO (forever (housekeepSessions cfg))
    return
      SessionManager
        { sm_getSessionId = enabled $ getSessionIdImpl vaultKey cfg sif,
          sm_getCsrfToken = enabled $ getCsrfTokenImpl vaultKey cfg sif,
          sm_regenerateSessionId = enabled $ regenerateSessionIdImpl vaultKey store cfg sif,
          sm_destroySession = enabled $ destroySessionImpl vaultKey store cfg sif,
          sm_readSession = enabled $ readSessionImpl vaultKey cfg sif,
          sm_writeSession = \value -> enabled $ writeSessionImpl vaultKey store cfg sif value,
          sm_modifySession = \f -> enabled $ modifySessionImpl vaultKey store cfg sif f,
          sm_clearAllSessions = enabled $ clearAllSessionsImpl store,
          sm_mapSessions = \f -> enabled $ mapAllSessionsImpl store f,
          sm_middleware = sessionMiddleware cfg vaultKey cookieKey,
          sm_closeSessionManager = mapM_ killThread housekeepThread
        }
  where
    store = sc_store cfg
    enabled :: MonadIO n => n a -> n a
    enabled action
      | sc_sessionMode cfg == SessionsDisabled = liftIO $ throwIO SessionUseWhenDisabled
      | otherwise = action

regenerateSessionIdImpl ::
  MonadIO m =>
  V.Key SessionId ->
  SessionStoreInstance (Session conn sess st) ->
  SessionCfg conn sess st ->
  SessionIf m ->
  m ()
regenerateSessionIdImpl vK sessionRef cfg sif =
  do
    sid <- si_queryVault sif vK
    fresh <- liftIO $ createSession cfg (sc_emptySession cfg)
    now <- liftIO getCurrentTime
    newSession <- liftIO $ case sessionRef of
      SessionStoreInstance store -> ss_runTx store $ do
        previous <- maybe (pure Nothing) (\key -> loadSessionTx cfg store key now) sid
        let replacement = fresh { sess_data = maybe (sc_emptySession cfg) sess_data previous }
        mapM_ (ss_deleteSession store) sid
        ss_storeSession store replacement
        pure replacement
    si_setRawMultiHeader sif MultiHeaderSetCookie (makeSessionIdCookie cfg newSession now)
    si_modifyVault sif $ V.insert vK (sess_id newSession)

destroySessionImpl :: MonadIO m => V.Key SessionId -> SessionStoreInstance (Session conn sess st) -> SessionCfg conn sess st -> SessionIf m -> m ()
destroySessionImpl vK store cfg sif = do
  sid <- si_queryVault sif vK
  liftIO $ mapM_ (deleteSessionImpl store) sid
  si_modifyVault sif $ V.delete vK
  now <- liftIO getCurrentTime
  let settings = (sc_cookieSettings cfg) { cs_EOL = CookieValidFor 0 }
  si_setRawMultiHeader sif MultiHeaderSetCookie $
    generateCookieHeaderString (sc_cookieName cfg) "" settings now

getSessionIdImpl ::
  MonadIO m =>
  V.Key SessionId ->
  SessionCfg conn sess st ->
  SessionIf m ->
  m SessionId
getSessionIdImpl vK cfg sif =
  do
    sess <- readSessionBase vK cfg sif
    return $ sess_id sess

getCsrfTokenImpl ::
  (MonadIO m) =>
  V.Key SessionId ->
  SessionCfg conn sess st ->
  SessionIf m ->
  m T.Text
getCsrfTokenImpl vK cfg sif =
  do
    sess <- readSessionBase vK cfg sif
    return $ sess_csrfToken sess

modifySessionBase ::
  MonadIO m =>
  V.Key SessionId ->
  SessionStoreInstance (Session conn sess st) ->
  SessionCfg conn sess st ->
  SessionIf m ->
  (Session conn sess st -> (Session conn sess st, a)) ->
  m a
modifySessionBase vK (SessionStoreInstance sessionRef) cfg sif modFun =
  do
    mValue <- si_queryVault sif vK
    now <- liftIO getCurrentTime
    mResult <-
      liftIO $ ss_runTx sessionRef $
        do
          mSession <- maybe (pure Nothing) (\sid -> loadSessionTx cfg sessionRef sid now) mValue
          forM mSession $ \session ->
            do
              let (sessionNew, result) = modFun session
              ss_storeSession sessionRef sessionNew
              return result
    case mResult of
      Just result -> return result
      Nothing ->
        do
          session <- liftIO $ createSession cfg (sc_emptySession cfg)
          let (sessionNew, result) = modFun session
          liftIO $ ss_runTx sessionRef $ ss_storeSession sessionRef sessionNew
          cookieTime <- liftIO getCurrentTime
          si_setRawMultiHeader sif MultiHeaderSetCookie (makeSessionIdCookie cfg sessionNew cookieTime)
          si_modifyVault sif $ V.insert vK (sess_id sessionNew)
          return result

readSessionBase ::
  MonadIO m =>
  V.Key SessionId ->
  SessionCfg conn sess st ->
  SessionIf m ->
  m (Session conn sess st)
readSessionBase vK cfg sif =
  do
    mValue <- si_queryVault sif vK
    readOrNewSession cfg vK sif mValue

readSessionImpl ::
  MonadIO m =>
  V.Key SessionId ->
  SessionCfg conn sess st ->
  SessionIf m ->
  m sess
readSessionImpl vK cfg sif =
  do
    base <- readSessionBase vK cfg sif
    return (sess_data base)

writeSessionImpl ::
  MonadIO m =>
  V.Key SessionId ->
  SessionStoreInstance (Session conn sess st) ->
  SessionCfg conn sess st ->
  SessionIf m ->
  sess ->
  m ()
writeSessionImpl vK sessionRef cfg sif value =
  modifySessionImpl vK sessionRef cfg sif (const (value, ()))

modifySessionImpl ::
  MonadIO m =>
  V.Key SessionId ->
  SessionStoreInstance (Session conn sess st) ->
  SessionCfg conn sess st ->
  SessionIf m ->
  (sess -> (sess, a)) ->
  m a
modifySessionImpl vK sessionRef cfg sif f =
  do
    let modFun session =
          let (sessData', out) = f (sess_data session)
           in (session {sess_data = sessData'}, out)
    modifySessionBase vK sessionRef cfg sif modFun

makeSessionIdCookie :: SessionCfg conn sess st -> Session conn sess st -> UTCTime -> BS.ByteString
makeSessionIdCookie cfg sess = generateCookieHeaderString name value settings
  where
    name = sc_cookieName cfg
    value = sess_id sess
    settings = sc_cookieSettings cfg

readOrNewSession ::
  MonadIO m =>
  SessionCfg conn sess st ->
  V.Key SessionId ->
  SessionIf m ->
  Maybe SessionId ->
  m (Session conn sess st)
readOrNewSession cfg vK sif mSid =
  do
    (sess, write) <- loadOrSpanSession cfg mSid
    when write $
      do
        now <- liftIO getCurrentTime
        si_setRawMultiHeader sif MultiHeaderSetCookie (makeSessionIdCookie cfg sess now)
        si_modifyVault sif $ V.insert vK (sess_id sess)
    return sess

loadOrSpanSession ::
  MonadIO m =>
  SessionCfg conn sess st ->
  Maybe SessionId ->
  m (Session conn sess st, Bool)
loadOrSpanSession cfg mSid =
  do
    mSess <-
      liftIO $
        join <$> T.mapM (loadSessionImpl cfg sessionRef) mSid
    case mSess of
      Nothing ->
        do
          newSess <-
            liftIO $
              newSessionImpl cfg sessionRef (sc_emptySession cfg)
          return (newSess, True)
      Just s -> return (s, False)
  where
    sessionRef = sc_store cfg

sessionMiddleware ::
  SessionCfg conn sess st ->
  V.Key SessionId ->
  V.Key (IORef (Maybe BS.ByteString)) ->
  Wai.Middleware
sessionMiddleware cfg vK cookieKey app req respond
  | sc_sessionMode cfg == SessionsDisabled = app req respond
  | otherwise = do
      (sid, pendingCookie) <- case sc_sessionMode cfg of
        SessionsAlways -> do
          (sess, writeCookie) <- loadOrSpanSession cfg cookieId
          now <- getCurrentTime
          pure (Just $ sess_id sess, if writeCookie then Just (makeSessionIdCookie cfg sess now) else Nothing)
        _ -> pure (cookieId, Nothing)
      pending <- newIORef pendingCookie
      let requestVault = V.insert cookieKey pending $ maybe v (\key -> V.insert vK key v) sid
      app (req { Wai.vault = requestVault }) $ \response -> do
        cookie <- readIORef pending
        respond $ maybe response (\value -> mapReqHeaders (("Set-Cookie", value) :) response) cookie
  where
    cookieId = getCookieFromReq (sc_cookieName cfg)
    getCookieFromReq name =
      lookup "cookie" (Wai.requestHeaders req) >>= lookup name . parseCookies
    v = Wai.vault req

newSessionImpl ::
  SessionCfg conn sess st ->
  SessionStoreInstance (Session conn sess st) ->
  sess ->
  IO (Session conn sess st)
newSessionImpl sessCfg (SessionStoreInstance sessionRef) content =
  do
    sess <- createSession sessCfg content
    ss_runTx sessionRef $ ss_storeSession sessionRef sess
    return $! sess

loadSessionImpl ::
  SessionCfg conn sess st ->
  SessionStoreInstance (Session conn sess st) ->
  SessionId ->
  IO (Maybe (Session conn sess st))
loadSessionImpl sessCfg (SessionStoreInstance store) sid =
  do
    now <- getCurrentTime
    ss_runTx store $
      do
        mSess <- loadSessionTx sessCfg store sid now
        when (sc_sessionExpandTTL sessCfg) $
          forM_ mSess (ss_storeSession store)
        return mSess

-- The caller must store a renewed or modified session in this same transaction.
loadSessionTx ::
  Monad tx =>
  SessionCfg conn sess st ->
  SessionStore (Session conn sess st) tx ->
  SessionId ->
  UTCTime ->
  tx (Maybe (Session conn sess st))
loadSessionTx sessCfg store sid now =
  do
    mSess <- ss_loadSession store sid
    case mSess of
      Just sess
        | sess_validUntil sess <= now ->
            do
              ss_deleteSession store sid
              return Nothing
        | sc_sessionExpandTTL sessCfg ->
            return $ Just $
              sess
                { sess_validUntil =
                    max (sess_validUntil sess) (addUTCTime (sc_sessionTTL sessCfg) now)
                }
        | otherwise -> return (Just sess)
      Nothing -> return Nothing

deleteSessionImpl ::
  SessionStoreInstance (Session conn sess st) ->
  SessionId ->
  IO ()
deleteSessionImpl (SessionStoreInstance sessionRef) sid =
  ss_runTx sessionRef $ ss_deleteSession sessionRef sid

clearAllSessionsImpl ::
  MonadIO m =>
  SessionStoreInstance (Session conn sess st) ->
  m ()
clearAllSessionsImpl (SessionStoreInstance sessionRef) =
  liftIO $ ss_runTx sessionRef $ ss_filterSessions sessionRef (const False)

mapAllSessionsImpl ::
  MonadIO m =>
  SessionStoreInstance (Session conn sess st) ->
  (forall n. Monad n => sess -> n sess) ->
  m ()
mapAllSessionsImpl (SessionStoreInstance sessionRef) f =
  liftIO $
    ss_runTx sessionRef $
      ss_mapSessions sessionRef $ \sess ->
        do
          newData <- f (sess_data sess)
          return $ sess {sess_data = newData}

housekeepSessions :: SessionCfg conn sess st -> IO ()
housekeepSessions cfg =
  case sc_store cfg of
    SessionStoreInstance store ->
      do
        now <- getCurrentTime
        (newStatus, oldStatus) <-
          ss_runTx store $
            do
              oldSt <- ss_toList store
              ss_filterSessions store (\sess -> sess_validUntil sess > now)
              (,) <$> ss_toList store <*> pure oldSt
        let packSessionHm = HM.fromList . map (\v -> (sess_id v, v))
            oldHm = packSessionHm oldStatus
            newHm = packSessionHm newStatus
        sh_removed (sc_hooks cfg) (HM.map sess_data $ oldHm `HM.difference` newHm)
        threadDelay (1000 * 1000 * (round $ sc_housekeepingInterval cfg))

createSession :: SessionCfg conn sess st -> sess -> IO (Session conn sess st)
createSession sessCfg content =
  do
    sid <- randomHash (sc_sessionIdEntropy sessCfg)
    csrfToken <- randomHash 12
    now <- getCurrentTime
    let validUntil = addUTCTime (sc_sessionTTL sessCfg) now
    return (Session sid csrfToken validUntil content)

randomHash :: Int -> IO T.Text
randomHash len =
  do
    by <- CR.getRandomBytes len
    return $
      T.replace "=" "" $
        T.replace "/" "_" $
          T.replace "+" "-" $
            T.decodeUtf8 $ B64.encode by
