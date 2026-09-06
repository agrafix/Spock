{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Spock.SessionCookieSpec (spec) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Network.HTTP.Types (HeaderName, status200, status403)
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai
import Test.Hspec
import Web.Spock
import Web.Spock.Config
import Web.Spock.Internal.SessionManager (Session (..))
import Web.Spock.Internal.SessionVault (newStmSessionStore')
import Web.Spock.TestUtils

spec :: Spec
spec = do
  forM_ [SessionsAlways, SessionsOnDemand] $ \mode -> describe ("Session cookies: " ++ show mode) $ do
    it "sends only the final ID when regenerating on the first request" $ do
      (app, store) <- cookieApp mode Nothing
      response <- send app "GET" "/regenerate" []
      sid <- requireCookie response
      Wai.simpleBody response `shouldBe` LBS.fromStrict sid
      length (sessionCookies response) `shouldBe` 1
      fmap sess_id <$> ss_runTx store (ss_toList store) `shouldReturn` [T.decodeUtf8 sid]

    it "revokes only the current session and makes its old cookie unusable" $ do
      (app, store) <- cookieApp mode Nothing
      now <- getCurrentTime
      ss_runTx store $ ss_storeSession store (Session "other-user" "other-token" (addUTCTime 60 now) 42)
      sid <- requireCookie =<< send app "GET" "/write" []
      response <- send app "POST" "/logout" [("Cookie", "spockcookie=" <> sid)]
      sessionCookies response `shouldSatisfy` oneExpiredCookie
      fmap sess_id <$> ss_runTx store (ss_toList store) `shouldReturn` ["other-user"]
      replay <- send app "GET" "/read" [("Cookie", "spockcookie=" <> sid)]
      Wai.simpleBody replay `shouldBe` "0"

    forM_ ["/logout", "/logout-twice", "/write-logout"] $ \path ->
      it ("leaves no replacement session or live cookie at " ++ show path) $ do
        (app, store) <- cookieApp mode Nothing
        response <- send app "POST" path []
        sessionCookies response `shouldSatisfy` oneExpiredCookie
        length <$> ss_runTx store (ss_toList store) `shouldReturn` 0

    it "creates a fresh session if the handler uses sessions after logout" $ do
      (app, store) <- cookieApp mode Nothing
      old <- requireCookie =<< send app "GET" "/write" []
      response <- send app "POST" "/logout-write" [("Cookie", "spockcookie=" <> old)]
      fresh <- requireCookie response
      fresh `shouldNotBe` old
      length (sessionCookies response) `shouldBe` 1
      contents <- ss_runTx store $ ss_toList store
      map sess_data contents `shouldBe` [9]
      readBack <- send app "GET" "/read" [("Cookie", "spockcookie=" <> fresh)]
      Wai.simpleBody readBack `shouldBe` "9"

    it "expires cookies with the same path, domain, and security attributes" $ do
      let settings = defaultCookieSettings { cs_path = Just "/app", cs_domain = Just "example.org",
                                            cs_secure = True, cs_HTTPOnly = True, cs_sameSite = Just SameSiteStrict }
      (app, _) <- cookieApp mode (Just settings)
      response <- send app "POST" "/logout" []
      case sessionCookies response of
        [cookieHeader] -> forM_ ["Path=/app", "Domain=example.org", "Secure", "HttpOnly", "SameSite=Strict", "Max-Age=0"] $ \attribute ->
          cookieHeader `shouldSatisfy` BS.isInfixOf attribute
        headers -> expectationFailure $ "Expected one expired cookie: " ++ show headers

  describe "Browser configuration" $ do
    it "opts in to CSRF and HTTPS browser-session cookies" $ do
      cfg <- defaultBrowserSpockCfg () PCNoDatabase ()
      let sessions = spc_sessionCfg cfg
          settings = sc_cookieSettings sessions
      spc_csrfProtection cfg `shouldBe` True
      sc_sessionMode sessions `shouldBe` SessionsOnDemand
      cs_secure settings `shouldBe` True
      cs_HTTPOnly settings `shouldBe` True
      cs_sameSite settings `shouldBe` Just SameSiteLax
      case cs_EOL settings of
        CookieValidForSession -> pure ()
        _ -> expectationFailure "Expected browser-session lifetime"

    it "protects logout and rejects the revoked token after logout" $ do
      cfg <- defaultBrowserSpockCfg () PCNoDatabase ()
      app <- spockAsApp $ spock cfg $ do
        get "token" $ getCsrfToken >>= text
        post "logout" $ sessionDestroy >> text "bye"
      rejected <- send app "POST" "/logout" []
      Wai.simpleStatus rejected `shouldBe` status403
      tokenResponse <- send app "GET" "/token" []
      sid <- requireCookie tokenResponse
      let headers = [("Cookie", "spockcookie=" <> sid), ("X-Csrf-Token", LBS.toStrict $ Wai.simpleBody tokenResponse)]
      response <- send app "POST" "/logout" headers
      Wai.simpleStatus response `shouldBe` status200
      sessionCookies response `shouldSatisfy` oneExpiredCookie
      replay <- send app "POST" "/logout" headers
      Wai.simpleStatus replay `shouldBe` status403

type Store = SessionStore (Session () Int ()) STM

cookieApp :: SessionMode -> Maybe CookieSettings -> IO (Wai.Application, Store)
cookieApp mode settings = do
  cfg <- defaultSpockCfg (0 :: Int) PCNoDatabase ()
  store <- newStmSessionStore'
  ready <- newEmptyMVar
  let sessions = (spc_sessionCfg cfg)
        { sc_sessionMode = mode, sc_store = SessionStoreInstance store,
          sc_cookieSettings = maybe (sc_cookieSettings $ spc_sessionCfg cfg) id settings,
          sc_hooks = SessionHooks (const $ putMVar ready ()) }
  app <- spockAsApp $ spock (cfg { spc_sessionCfg = sessions }) $ do
    get "regenerate" $ sessionRegenerateId >> getSessionId >>= text
    get "write" $ writeSession 7 >> text "ok"
    get "read" $ readSession >>= text . T.pack . show
    post "logout" $ sessionDestroy >> text "bye"
    post "logout-twice" $ sessionDestroy >> sessionDestroy >> text "bye"
    post "write-logout" $ writeSession 7 >> sessionDestroy >> text "bye"
    post "logout-write" $ sessionDestroy >> writeSession 9 >> text "ok"
  takeMVar ready
  pure (app, store)

sessionCookies :: Wai.SResponse -> [BS.ByteString]
sessionCookies = map snd . filter ((== "Set-Cookie") . fst) . Wai.simpleHeaders

oneExpiredCookie :: [BS.ByteString] -> Bool
oneExpiredCookie [value] = "spockcookie=;" `BS.isPrefixOf` value && "Max-Age=0" `BS.isInfixOf` value
oneExpiredCookie _ = False

requireCookie :: Wai.SResponse -> IO BS.ByteString
requireCookie response = maybe (fail "Missing session cookie") (pure . T.encodeUtf8) $ getSessCookie response

send :: Wai.Application -> BS.ByteString -> BS.ByteString -> [(HeaderName, BS.ByteString)] -> IO Wai.SResponse
send app method path headers = Wai.runSession
  (Wai.srequest $ Wai.SRequest ((Wai.setPath Wai.defaultRequest path) { Wai.requestMethod = method, Wai.requestHeaders = headers }) "") app
