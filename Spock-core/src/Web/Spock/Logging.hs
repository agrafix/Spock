{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Request correlation and structured logging, independent of any logger
-- library. Enable through 'Web.Spock.Core.sc_logging' or use the middleware
-- directly in a WAI application.
module Web.Spock.Logging
  ( LoggingConfig (..), defaultLoggingConfig,
    RequestContext (..), LogLevel (..), LogEvent (..), LogEventType (..),
    RequestLogger, newRequestLogger, requestLoggingMiddleware, lookupRequestLogger,
    emitRequestLog,
  ) where

import Control.Exception
import qualified System.Entropy as Entropy
import Data.Aeson (ToJSON (..), Value, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.IO as T
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Vault.Lazy as Vault
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Types (HeaderName, statusCode)
import qualified Network.Wai as Wai
import System.IO (stderr)

data RequestContext = RequestContext
  { rc_requestId :: Text,
    rc_method :: Text,
    -- | Raw path only; excludes query strings, headers, and request bodies.
    rc_path :: Text
  } deriving (Eq, Show)

data LogLevel = LogDebug | LogInfo | LogWarning | LogError deriving (Eq, Show)

data LogEventType
  = -- | Status and elapsed microseconds until the response is available.
    -- Streaming body transmission is not included in the duration.
    AccessLog Int Word64
  | MessageLog LogLevel Text [(Text, Value)]
  | ErrorLog Text
  deriving (Eq, Show)

data LogEvent = LogEvent
  { le_time :: UTCTime,
    le_request :: RequestContext,
    le_event :: LogEventType
  } deriving (Eq, Show)

instance ToJSON LogEvent where
  toJSON event = object $
    [ "time" .= le_time event,
      "requestId" .= rc_requestId context,
      "method" .= rc_method context,
      "path" .= rc_path context ] ++ details
    where
      context = le_request event
      details = case le_event event of
        AccessLog status duration -> ["type" .= ("access" :: Text), "status" .= status, "durationMicros" .= duration]
        MessageLog level message fields ->
          ["type" .= ("message" :: Text), "level" .= levelName level, "message" .= message,
           "fields" .= object [Key.fromText key .= value | (key, value) <- fields]]
        ErrorLog message -> ["type" .= ("error" :: Text), "level" .= ("error" :: Text), "message" .= message]

levelName :: LogLevel -> Text
levelName LogDebug = "debug"
levelName LogInfo = "info"
levelName LogWarning = "warning"
levelName LogError = "error"

data LoggingConfig = LoggingConfig
  { lc_logEvent :: LogEvent -> IO (),
    lc_requestIdHeader :: HeaderName,
    -- | Opt in only behind a trusted proxy. Accepted values contain 1-128 ASCII
    -- letters, digits, dots, underscores, or hyphens; duplicates are rejected.
    lc_trustIncomingRequestId :: Bool,
    lc_generateRequestId :: IO Text,
    -- | Called when the sink fails. Synchronous failures in logging callbacks
    -- cannot change responses. Asynchronous cancellation is always propagated.
    lc_logFailure :: SomeException -> IO ()
  }

defaultLoggingConfig :: (LogEvent -> IO ()) -> LoggingConfig
defaultLoggingConfig sink = LoggingConfig sink "X-Request-Id" False generateId
  (\_ -> T.hPutStrLn stderr "Spock: structured log sink failed")

data RequestLogger = RequestLogger LoggingConfig (Vault.Key RequestContext)

newRequestLogger :: LoggingConfig -> IO RequestLogger
newRequestLogger cfg = RequestLogger cfg <$> Vault.newKey

lookupRequestLogger :: RequestLogger -> Wai.Request -> Maybe (RequestContext, LogEventType -> IO ())
lookupRequestLogger logger@(RequestLogger _ key) request = do
  context <- Vault.lookup key (Wai.vault request)
  pure (context, emitRequestLog logger context)

emitRequestLog :: RequestLogger -> RequestContext -> LogEventType -> IO ()
emitRequestLog (RequestLogger cfg _) context event = do
  now <- getCurrentTime
  lc_logEvent cfg (LogEvent now context event) `catch` \(err :: SomeException) -> do
    rethrowAsync err
    lc_logFailure cfg err `catch` \(failure :: SomeException) -> rethrowAsync failure

requestLoggingMiddleware :: RequestLogger -> Wai.Middleware
requestLoggingMiddleware logger@(RequestLogger cfg key) app request respond
  -- Error handlers can enter Spock again with the same request.
  | Just _ <- Vault.lookup key (Wai.vault request) = app request respond
  | otherwise = do
      sid <- case [value | (name, value) <- Wai.requestHeaders request, name == lc_requestIdHeader cfg] of
        [value] | lc_trustIncomingRequestId cfg, validId value -> pure $ T.decodeUtf8 value
        _ -> do
          generated <- lc_generateRequestId cfg
          if validId (T.encodeUtf8 generated) then pure generated else generateId
      let context = RequestContext sid (decode $ Wai.requestMethod request) (decode $ Wai.rawPathInfo request)
          request' = request { Wai.vault = Vault.insert key context (Wai.vault request) }
      start <- getMonotonicTimeNSec
      app request' (\response -> do
        end <- getMonotonicTimeNSec
        emitRequestLog logger context $ AccessLog (statusCode $ Wai.responseStatus response) ((end - start) `div` 1000)
        respond $ Wai.mapResponseHeaders
          (\headers -> (lc_requestIdHeader cfg, T.encodeUtf8 sid) : filter ((/= lc_requestIdHeader cfg) . fst) headers) response)
        `catch` \(err :: SomeException) -> do
          rethrowAsync err
          emitRequestLog logger context $ ErrorLog (T.pack $ displayException err)
          throwIO err
  where
    decode = T.decodeUtf8With lenientDecode

validId :: BS.ByteString -> Bool
validId value = not (BS.null value) && BS.length value <= 128 && BS.all allowed value
  where
    allowed c = (c >= 65 && c <= 90) || (c >= 97 && c <= 122) || (c >= 48 && c <= 57) || c `elem` [45, 46, 95]

generateId :: IO Text
generateId = do
  bytes <- Entropy.getEntropy 16
  pure $ T.decodeUtf8 $ LBS.toStrict $ Builder.toLazyByteString $
    foldMap Builder.word8HexFixed (BS.unpack bytes)

rethrowAsync :: SomeException -> IO ()
rethrowAsync err = case fromException err :: Maybe SomeAsyncException of
  Just _ -> throwIO err
  Nothing -> pure ()
