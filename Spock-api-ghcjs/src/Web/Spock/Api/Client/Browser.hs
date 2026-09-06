{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Browser Fetch transport for GHC's JavaScript backend. Same-origin cookies
-- are the default. Supply CSRF headers explicitly for unsafe cookie-authenticated
-- endpoints. Cross-origin calls require the server's CORS policy and, for
-- cross-origin cookies, IncludeCredentials. Redirects are rejected.
module Web.Spock.Api.Client.Browser (browserClient) where

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.JS.Prim
import Web.Spock.Api.Client

-- | Create a client with an AbortController timeout and a streaming response
-- limit. Network failures, timeout, response size, HTTP status and JSON errors
-- are distinct ClientError values. No response data or credentials are logged.
browserClient :: ClientConfig -> Either ClientError Client
browserClient cfg = newClient cfg browserTransport

browserTransport :: Transport
browserTransport request = do
  let configuration = object
        [ "method" .= rq_method request, "url" .= rq_url request,
          "headers" .= [(T.decodeLatin1 n, T.decodeLatin1 v) | (n, v) <- rq_headers request],
          "body" .= fmap T.decodeUtf8 (rq_body request),
          "credentials" .= credentials (rq_credentials request),
          "timeout" .= rq_timeoutMilliseconds request,
          "maxBytes" .= rq_maxResponseBytes request ]
      argument = toJSString $ T.unpack $ T.decodeUtf8 $ BL.toStrict $ encode configuration
  result <- js_fetch argument
  errorCode <- fromJSString <$> getProp result "error"
  case errorCode of
    "timeout" -> pure $ Left RequestTimedOut
    "large" -> pure $ Left ResponseTooLarge
    "decode" -> pure $ Left DecodeFailure
    "" -> do
      status <- fromJSInt <$> getProp result "status"
      body <- fromJSString <$> getProp result "body"
      pure $ Right $ Response status (T.encodeUtf8 $ T.pack body)
    _ -> pure $ Left NetworkFailure
  where
    credentials :: Credentials -> T.Text
    credentials SameOrigin = "same-origin"
    credentials OmitCredentials = "omit"
    credentials IncludeCredentials = "include"

foreign import javascript interruptible "h$spock_fetch"
  js_fetch :: JSVal -> IO JSVal
