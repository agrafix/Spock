{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.MVar
import Control.Exception (bracket_)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.JS.Foreign.Callback
import GHC.JS.Prim
import Shared
import Web.Spock.Api.Client
import Web.Spock.Api.Client.Browser

main :: IO ()
main = do
  client <- either (fail . show) pure $ browserClient defaultClientConfig
    { cc_timeoutMilliseconds = 2000, cc_maxResponseBytes = 4096 }
  lock <- newMVar ()
  csrf <- callEndpoint client getCsrf
  case csrf of
    Left err -> report err
    Right token -> do
      let headers = [("X-Csrf-Token", token)]
          refresh = do
            result <- callEndpoint client getNote
            case result of
              Left err -> report err
              Right note -> setText "note" $ maybe "No note yet" id note
          action request = withMVar lock $ \() -> bracket_ (js_busy True) (js_busy False) $ do
            clearError
            result <- request
            either report (const refresh) result
          input = T.pack . fromJSString <$> js_input
      onClick "create" $ action $ input >>= callEndpoint' client createNote headers
      onClick "replace" $ action $ input >>= callEndpoint' client replaceNote headers
      onClick "append" $ action $ input >>= callEndpoint' client appendNote headers
      onClick "delete" $ action $ callEndpoint' client deleteNote headers
      onClick "load" $ withMVar lock $ \() -> clearError >> refresh
      onClick "echo" $ withMVar lock $ \() -> do
        clearError
        result <- callDocumentedEndpoint client echo "report/a.b λ😀" "a+b&λ" (Just 2) ["first", "two words"] "browser" (Just "optional")
        either report (setText "echo-output" . T.decodeUtf8 . BL.toStrict . encode) result
      refresh
      js_busy False
      js_ready

onClick :: T.Text -> IO () -> IO ()
onClick name action = do
  -- Each callback lives as long as its page. Dynamic components should remove
  -- listeners and releaseCallback when unmounted; see GHC.JS.Foreign.Callback.
  callback <- asyncCallback action
  js_click (toJSString $ T.unpack name) callback

setText :: T.Text -> T.Text -> IO ()
setText name value = js_text (toJSString $ T.unpack name) (toJSString $ T.unpack value)

clearError :: IO ()
clearError = js_error (toJSString "") (toJSString "")

report :: ClientError -> IO ()
report err = js_error (toJSString $ show err) (toJSString message)
  where
    message = case err of
      HttpError status -> "Server returned HTTP " ++ show status ++ ". Reload if your session has expired."
      RequestTimedOut -> "The request timed out. Try again."
      ResponseTooLarge -> "The server response is too large."
      DecodeFailure -> "The server returned an invalid response."
      NetworkFailure -> "Could not reach the server. Try again."
      _ -> "Could not prepare the request."

foreign import javascript unsafe "(() => document.getElementById('input').value)"
  js_input :: IO JSVal
foreign import javascript unsafe "((id, callback) => document.getElementById(id).addEventListener('click', callback))"
  js_click :: JSVal -> Callback (IO ()) -> IO ()
foreign import javascript unsafe "((id, value) => { document.getElementById(id).textContent = value; })"
  js_text :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe "((busy) => document.querySelectorAll('button').forEach(b => { b.disabled = busy; }))"
  js_busy :: Bool -> IO ()
foreign import javascript unsafe "((code, message) => { const el = document.getElementById('status'); el.dataset.error = code; el.textContent = message; })"
  js_error :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe "(() => { document.documentElement.dataset.ready = 'true'; })"
  js_ready :: IO ()
