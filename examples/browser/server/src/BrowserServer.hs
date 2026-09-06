{-# LANGUAGE OverloadedStrings #-}

module BrowserServer (makeApp) where

import Control.Monad (unless)
import Control.Monad.IO.Class
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai (Application)
import Shared
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import qualified Web.Spock as S
import Web.Spock.Api.Server
import Web.Spock.Config

-- | The caller selects HTTPS cookies or explicit local HTTP development.
-- Assets must be the output of scripts/build-browser.py. Serve only known files.
makeApp :: Bool -> FilePath -> IO Application
makeApp secure assets = do
  cfg <- defaultBrowserSpockCfg (Nothing :: Maybe T.Text) PCNoDatabase ()
  let session = spc_sessionCfg cfg
      configured = cfg { spc_sessionCfg = session
        { sc_cookieSettings = (sc_cookieSettings session) { cs_secure = secure } } }
  S.spockAsApp $ S.spock configured $ do
    S.get S.root $ S.redirect "/app/"
    S.get ("app" S.<//> S.wildcard) $ \_ ->
      S.setHeader "Cache-Control" "no-store" >> S.file "text/html;charset=utf-8" (assets </> "index.html")
    S.get "all.js" $ S.setHeader "Cache-Control" "no-cache" >> S.file "text/javascript;charset=utf-8" (assets </> "all.js")
    -- GHC's C-library support may emit additional JS/wasm files.
    S.get "clibs.js" $ serve "text/javascript" "clibs.js"
    S.get "clibs.wasm" $ serve "application/wasm" "clibs.wasm"
    defEndpoint getCsrf $ noCache >> S.getCsrfToken
    defEndpoint getNote $ noCache >> S.readSession
    defDocumentedEndpoint echo $ \name search offset tags caller optional ->
      pure $ Echo name search offset tags caller optional
    -- These helpers register through Spock-core, so protect their unsafe
    -- methods explicitly. The Spock configuration flag alone does not wrap them.
    S.prehook S.csrfCheck $ do
      defEndpoint createNote $ \value -> do
        checkValue value
        S.writeSession (Just value)
        S.setStatus status201
        pure value
      defEndpoint replaceNote $ \value -> checkValue value >> S.writeSession (Just value) >> pure value
      defEndpoint appendNote $ \value -> do
        current <- S.readSession
        case current of
          Nothing -> S.setStatus status404 >> S.json ("No note" :: T.Text)
          Just old -> do
            let updated = old <> value
            checkValue updated
            S.writeSession (Just updated)
            pure updated
      defEndpoint deleteNote $ S.writeSession Nothing >> pure True
  where
    noCache = S.setHeader "Cache-Control" "no-store"
    checkValue value = unless (not (T.null value) && T.length value <= 200) $
      S.setStatus status400 >> S.json ("Use 1 to 200 characters" :: T.Text)
    serve content name = do
      exists <- liftIO $ doesFileExist (assets </> name)
      unless exists $ S.setStatus status404 >> S.text "Not found"
      S.file content (assets </> name)
