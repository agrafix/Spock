{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Web.Spock.Internal.ClientSession (createClientSessionManager) where

import Control.Concurrent.MVar
import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import Data.Time
import qualified Data.Vault.Lazy as V
import Network.HTTP.Types.URI (urlDecode)
import qualified Network.Wai as Wai
import qualified Web.Cookie as Cookie
import Web.Spock.Internal.Cookies
import Web.Spock.Internal.SessionCommon
import Web.Spock.Internal.Types
import Web.Spock.Internal.Util (mapReqHeaders)

data ClientState conn sess st
  = Unloaded (Maybe BS.ByteString)
  | Loaded (Session conn sess st)
  | Destroyed

-- Both state and the pending response cookie commit together. An encoding or
-- size failure leaves the last successful session operation intact.
type ClientCell conn sess st = MVar (ClientState conn sess st, Maybe BS.ByteString)

createClientSessionManager :: MonadIO m => SessionCfg conn sess st -> ClientSessionCfg sess -> SessionIf m -> IO (SessionManager m conn sess st)
createClientSessionManager cfg client sif = do
  when (csc_maxCookieBytes client < 1 || csc_maxCookieBytes client > 4096
        || sc_sessionTTL cfg <= 0 || sc_sessionIdEntropy cfg < 16
        || sc_sessionIdEntropy cfg > 256) $
    throwIO InvalidClientSessionConfig
  key <- V.newKey
  let withCell action
        | sc_sessionMode cfg == SessionsDisabled = liftIO $ throwIO SessionUseWhenDisabled
        | otherwise = do
            cell <- si_queryVault sif key
            maybe (liftIO $ throwIO ClientSessionOutsideRequest) (liftIO . action) cell
      readCurrent = withCell $ operate cfg client False (\s -> pure (s, s))
      change f = withCell $ operate cfg client True f
      middleware app req respond
        | sc_sessionMode cfg == SessionsDisabled = app req respond
        | otherwise = do
            let raw = lookup "Cookie" (Wai.requestHeaders req) >>=
                  lookup (T.encodeUtf8 $ sc_cookieName cfg) . Cookie.parseCookies
                incoming = raw >>= \v -> if BS.length v <= csc_maxCookieBytes client
                  then Just (urlDecode False v) else Nothing
            cell <- newMVar (Unloaded incoming, Nothing)
            when (sc_sessionMode cfg == SessionsAlways) $
              operate cfg client False (\s -> pure (s, ())) cell
            app (req { Wai.vault = V.insert key cell (Wai.vault req) }) $ \response -> do
              (_, pending) <- readMVar cell
              respond $ maybe response (\v -> mapReqHeaders (("Set-Cookie", v) :) response) pending
  pure SessionManager
    { sm_getSessionId = sess_id <$> readCurrent,
      sm_getCsrfToken = sess_csrfToken <$> readCurrent,
      sm_readSession = sess_data <$> readCurrent,
      sm_writeSession = \value -> change $ \s -> pure (s { sess_data = value }, ()),
      sm_modifySession = \f -> change $ \s ->
        let (value, result) = f (sess_data s) in pure (s { sess_data = value }, result),
      sm_regenerateSessionId = change $ \s -> do
        now <- csc_clock client
        fresh <- createSessionAt cfg now (sess_data s)
        pure (fresh, ()),
      sm_destroySession = withCell $ \cell -> modifyMVar_ cell $ \_ -> do
        now <- csc_clock client
        let settings = (sc_cookieSettings cfg) { cs_EOL = CookieValidFor 0 }
        header <- checkedHeader client $ generateCookieHeaderString (sc_cookieName cfg) "" settings now
        pure (Destroyed, Just header),
      sm_serverSessions = Nothing,
      sm_middleware = middleware,
      sm_closeSessionManager = pure ()
    }

operate :: SessionCfg conn sess st -> ClientSessionCfg sess -> Bool ->
  (Session conn sess st -> IO (Session conn sess st, a)) -> ClientCell conn sess st -> IO a
operate cfg client changed f cell = modifyMVar cell $ \(state, pending) -> do
  now <- csc_clock client
  (session, reissue) <- resolve now state
  (updated, result) <- f session
  cookie <- if changed || reissue
    then do
      encoded <- csc_encode (csc_codec client) (sc_cookieName cfg) updated
      value <- either (const $ throwIO InvalidClientSessionConfig) pure (T.decodeUtf8' encoded)
      header <- checkedHeader client $ generateCookieHeaderString (sc_cookieName cfg) value (sc_cookieSettings cfg) now
      pure (Just header)
    else pure pending
  pure ((Loaded updated, cookie), result)
  where
    fresh now = do
      session <- createSessionAt cfg now (sc_emptySession cfg)
      pure (session, True)
    resolve now (Unloaded raw) = do
      decoded <- maybe (pure Nothing) (csc_decode (csc_codec client) (sc_cookieName cfg)) raw
      case decoded of
        Just (s, rotate) | sess_validUntil s > now ->
          if sc_sessionExpandTTL cfg
            then pure (s { sess_validUntil = max (sess_validUntil s) (addUTCTime (sc_sessionTTL cfg) now) }, True)
            else pure (s, rotate)
        _ -> fresh now
    resolve now (Loaded s)
      | sess_validUntil s > now = pure (s, False)
      | otherwise = fresh now
    resolve now Destroyed = fresh now

checkedHeader :: ClientSessionCfg sess -> BS.ByteString -> IO BS.ByteString
checkedHeader client header
  | BS.length header > csc_maxCookieBytes client = throwIO ClientSessionCookieTooLarge
  | otherwise = pure header
