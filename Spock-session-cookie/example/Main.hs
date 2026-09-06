{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp
import System.Environment (getArgs, lookupEnv)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import Web.Spock
import Web.Spock.Config
import Web.Spock.Session.Cookie

main :: IO ()
main = do
  args <- getArgs
  (secure, port) <- case args of
    [mode, number] | Just p <- readMaybe number, p >= 1, p <= 65535,
      Just secure <- lookup mode [("--local-http", False), ("--https", True)] -> pure (secure, p)
    _ -> fail "Usage: spock-cookie-example (--local-http|--https) PORT"
  encoded <- lookupEnv "SPOCK_COOKIE_KEY" >>= maybe (fail "Set SPOCK_COOKIE_KEY to a base64-encoded random 32-byte key") pure
  keyBytes <- either (const $ fail "Invalid base64 cookie key") pure (B64.decode $ B.pack encoded)
  key <- either (fail . show) pure $ cookieKey "current" keyBytes
  keys <- either (fail . show) pure $ cookieKeyRing "spock-cookie-example" key []
  cfg <- defaultBrowserSpockCfg (0 :: Int) PCNoDatabase ()
  let sessions = (spc_sessionCfg cfg)
        { sc_backend = ClientSessions $ defaultClientSessionCfg $ cookieSessionCodec keys,
          sc_sessionTTL = 900,
          sc_cookieSettings = (sc_cookieSettings $ spc_sessionCfg cfg) { cs_secure = secure } }
  app <- spockAsApp $ spock (cfg { spc_sessionCfg = sessions }) $ do
    get root $ do
      setHeader "Cache-Control" "no-store"
      html page
    get "csrf" $ do
      setHeader "Cache-Control" "no-store"
      getCsrfToken >>= text
    get "value" $ do
      setHeader "Cache-Control" "no-store"
      readSession >>= text . T.pack . show
    post "increment" $ modifyReadSession (+ 1) >>= text . T.pack . show
    post "reset" $ sessionDestroy >> text "Reset"
  hPutStrLn stderr $ "Listening on 127.0.0.1:" ++ show port
  Warp.runSettings (Warp.setHost "127.0.0.1" $ Warp.setPort port Warp.defaultSettings) app

page :: T.Text
page = T.unlines
  [ "<!doctype html><html lang='en'><meta charset='utf-8'><title>Cookie session example</title>",
    "<main><h1>Your counter</h1><p>Value: <output id='value'>Loading…</output></p>",
    "<button id='increment' disabled>Increment</button> <button id='reset' disabled>Reset</button>",
    "<p id='status' role='status'></p></main><script>",
    "const value = document.querySelector('#value'), status = document.querySelector('#status');",
    "const buttons = [...document.querySelectorAll('button')]; let csrf = '';",
    "async function request(path, method = 'GET') {",
    "  const r = await fetch('/' + path, {method, credentials: 'same-origin', cache: 'no-store',",
    "    headers: method === 'POST' ? {'X-Csrf-Token': csrf} : {}});",
    "  if (!r.ok) throw new Error('Request failed (' + r.status + '). Reload to try again.');",
    "  return r.text();",
    "}",
    "async function refresh() { csrf = await request('csrf'); value.textContent = await request('value'); }",
    "async function perform(action) { buttons.forEach(b => b.disabled = true); status.textContent = '';",
    "  try { await action(); } catch (e) { status.textContent = e.message; }",
    "  finally { buttons.forEach(b => b.disabled = false); }",
    "}",
    "document.querySelector('#increment').onclick = () => perform(async () => { value.textContent = await request('increment', 'POST'); });",
    "document.querySelector('#reset').onclick = () => perform(async () => { await request('reset', 'POST'); await refresh(); });",
    "perform(refresh);</script></html>"
  ]
