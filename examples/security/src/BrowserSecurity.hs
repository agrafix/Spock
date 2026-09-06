{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module BrowserSecurity (Deployment (..), makeApp) where

import Control.Exception (evaluate)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Password.Argon2
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Lucid as H
import Network.HTTP.Types
import qualified Network.Wai as Wai
import Web.Spock
import Web.Spock.Api (Endpoint (MethodPost), Proxy (Proxy))
import Web.Spock.Api.Server (defEndpoint)
import Web.Spock.Config

data Deployment = BehindHTTPS | LocalHTTP deriving (Eq, Show)

data LoginSession = LoginSession
  { userName :: T.Text,
    displayName :: T.Text
  }

type AppState = PasswordHash Argon2
type Action a = SpockAction () (Maybe LoginSession) AppState a

makeApp :: Deployment -> AppState -> IO Wai.Application
makeApp deployment passwordHash = do
  cfg <- defaultBrowserSpockCfg Nothing PCNoDatabase passwordHash
  let sessions = spc_sessionCfg cfg
      cookieSettings = (sc_cookieSettings sessions)
        { cs_secure = deployment == BehindHTTPS,
          cs_path = Just "/",
          cs_domain = Nothing }
      browserSessions = sessions
        { sc_cookieName = if deployment == BehindHTTPS then "__Host-spock" else "spockcookie",
          sc_cookieSettings = cookieSettings }
  spockAsApp $ spock (cfg { spc_sessionCfg = browserSessions, spc_maxRequestSize = Just (16 * 1024) }) routes

routes :: SpockM () (Maybe LoginSession) AppState ()
routes = do
  middleware securityHeaders
  get root $ do
    token <- getCsrfToken
    page "Sign in" $ H.form_ [H.method_ "post", H.action_ "/login"] $ do
      csrfInput token
      H.label_ $ do
        "User name"
        H.input_ [H.name_ "username", H.value_ "demo", H.autocomplete_ "username"]
      H.label_ $ do
        "Password"
        H.input_ [H.name_ "password", H.type_ "password", H.autocomplete_ "current-password"]
      H.button_ "Sign in"
  get "csrf" $ getCsrfToken >>= text
  post "login" $ do
    username <- postField "username"
    password <- postField "password"
    when (T.length password > 1024) $ setStatus status400 >> text "Password too long"
    passwordHash <- getState
    -- Evaluate the password check even for an unknown user name. The example
    -- has one account; a real user store should also avoid account enumeration.
    result <- liftIO $ evaluate $ checkPassword (mkPassword password) passwordHash
    unless (result == PasswordCheckSuccess && username == "demo") $
      setStatus status401 >> text "Invalid credentials"
    sessionRegenerateId
    writeSession $ Just $ LoginSession "demo" "demo"
    seeOther "/account"
  get "account" $ do
    member <- requireUser
    token <- getCsrfToken
    page "Account" $ do
      H.h1_ $ H.toHtml (displayName member)
      H.p_ $ H.toHtml ("Signed in as " <> userName member)
      H.form_ [H.method_ "post", H.action_ "/profile"] $ do
        csrfInput token
        H.label_ $ do
          "Display name"
          H.input_ [H.name_ "displayName", H.value_ $ displayName member, H.maxlength_ "80"]
        H.button_ "Save"
      H.form_ [H.method_ "post", H.action_ "/logout"] $ do
        csrfInput token
        H.button_ "Sign out"
  post "profile" $ do
    _ <- requireUser
    postField "displayName" >>= updateProfile
    seeOther "/account"
  post ("api" <//> "profile") $ do
    _ <- requireUser
    value <- jsonBody :: Action (Maybe T.Text)
    case value of
      Nothing -> setStatus status400 >> text "Expected a JSON string"
      Just name -> updateProfile name >> json name
  -- API-server registers through Spock-core. The browser configuration flag
  -- does not wrap its endpoints; apply CSRF explicitly to this POST endpoint.
  prehook csrfCheck $
    defEndpoint (MethodPost (Proxy :: Proxy (T.Text -> T.Text)) ("typed" <//> "profile")) $ \name -> do
      _ <- requireUser
      updateProfile name
      pure name
  post "logout" $ do
    sessionDestroy
    -- Do not read the session or ask for another token after destroying it.
    seeOther "/"

requireUser :: Action LoginSession
requireUser = readSession >>= maybe (setStatus status401 >> text "Sign in required") pure

updateProfile :: T.Text -> Action ()
updateProfile name = do
  when (T.null name || T.length name > 80) $ setStatus status400 >> text "Display name must have 1 to 80 characters"
  modifySession $ fmap $ \member -> member { displayName = name }

-- Credentials and form data come from the POST body, with duplicates rejected.
-- A query string cannot supply or override a login field.
postField :: T.Text -> Action T.Text
postField field = do
  values <- map snd . filter ((== field) . fst) <$> paramsPost
  case values of
    [value] -> pure value
    _ -> setStatus status400 >> text "Missing or duplicate form field"

seeOther :: T.Text -> Action a
seeOther location = setStatus status303 >> setHeader "Location" location >> text ""

page :: T.Text -> H.Html () -> Action a
page title content = html $ TL.toStrict $ H.renderText $ H.doctypehtml_ $ do
  H.head_ $ H.title_ $ H.toHtml title
  H.body_ content

csrfInput :: T.Text -> H.Html ()
csrfInput token = H.input_ [H.type_ "hidden", H.name_ "__csrf_token", H.value_ token]

securityHeaders :: Wai.Middleware
securityHeaders application req respond = application req $ respond . Wai.mapResponseHeaders (headers ++)
  where
    headers =
      [ ("Cache-Control", "no-store"),
        ("Content-Security-Policy", "default-src 'none'; connect-src 'self'; form-action 'self'; base-uri 'none'; frame-ancestors 'none'"),
        ("X-Content-Type-Options", "nosniff"),
        ("Referrer-Policy", "no-referrer")
      ]
