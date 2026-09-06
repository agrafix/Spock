{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import BrowserSecurity
import Control.Monad (forM_)
import Data.Aeson (encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Password.Argon2
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai
import Test.Hspec
import qualified Web.Cookie as Cookie

data Browser = Browser { sessionCookie :: Header, csrfToken :: BS.ByteString }

main :: IO ()
main = hspec $ beforeAll (hashPassword $ mkPassword testPassword) $ do
  beforeWith (makeApp BehindHTTPS) $ do
    describe "Browser security guide" $ do
      it "sets host-only HTTPS session cookies and browser security headers" $ \app -> do
        res <- send app "GET" "/" [] ""
        session <- responseCookie res
        Cookie.setCookieName session `shouldBe` "__Host-spock"
        Cookie.setCookiePath session `shouldBe` Just "/"
        Cookie.setCookieDomain session `shouldBe` Nothing
        Cookie.setCookieSecure session `shouldBe` True
        Cookie.setCookieHttpOnly session `shouldBe` True
        Cookie.setCookieSameSite session `shouldBe` Just Cookie.sameSiteLax
        Cookie.setCookieExpires session `shouldBe` Nothing
        lookup "Cache-Control" (Wai.simpleHeaders res) `shouldBe` Just "no-store"
        lookup "X-Content-Type-Options" (Wai.simpleHeaders res) `shouldBe` Just "nosniff"
        lookup "Content-Security-Policy" (Wai.simpleHeaders res) `shouldSatisfy`
          maybe False ("frame-ancestors 'none'" `BS.isInfixOf`)
        rejected <- send app "POST" "/login" [] ""
        Wai.simpleStatus rejected `shouldBe` status403
        lookup "Cache-Control" (Wai.simpleHeaders rejected) `shouldBe` Just "no-store"
        lookup "Content-Security-Policy" (Wai.simpleHeaders rejected) `shouldBe`
          lookup "Content-Security-Policy" (Wai.simpleHeaders res)
      it "rejects missing and invalid login CSRF tokens" $ \app -> do
        visitor <- openSession app
        forM_ [[], [("__csrf_token", "wrong")]] $ \fields -> do
          res <- form app "/login" visitor $ fields ++ credentials
          Wai.simpleStatus res `shouldBe` status403
      it "rejects a token stolen from another session" $ \app -> do
        victim <- openSession app
        attacker <- openSession app
        res <- form app "/login" attacker $ ("__csrf_token", csrfToken victim) : credentials
        Wai.simpleStatus res `shouldBe` status403
      it "requires valid credentials and never authenticates a rejected login" $ \app -> do
        visitor <- openSession app
        forM_ [[("username", "demo"), ("password", "wrong")], [("username", "unknown"), ("password", T.encodeUtf8 testPassword)]] $ \fields -> do
          res <- form app "/login" visitor $ tokenField visitor : fields
          Wai.simpleStatus res `shouldBe` status401
          Wai.simpleBody res `shouldBe` "Invalid credentials"
        statusFor app "GET" "/account" [sessionCookie visitor] "" `shouldReturn` status401
      it "rejects query-only and duplicate credential fields" $ \app -> do
        visitor <- openSession app
        queryOnly <- form app "/login?username=demo&password=irrelevant" visitor [tokenField visitor]
        Wai.simpleStatus queryOnly `shouldBe` status400
        duplicate <- form app "/login" visitor $ tokenField visitor : ("username", "other") : credentials
        Wai.simpleStatus duplicate `shouldBe` status400
      it "rotates the session ID and CSRF token before granting login privileges" $ \app -> do
        visitor <- openSession app
        (res, member) <- signIn app visitor
        Wai.simpleStatus res `shouldBe` status303
        lookup "Location" (Wai.simpleHeaders res) `shouldBe` Just "/account"
        sessionCookie member `shouldNotBe` sessionCookie visitor
        csrfToken member `shouldNotBe` csrfToken visitor
        statusFor app "GET" "/account" [sessionCookie member] "" `shouldReturn` status200
        statusFor app "GET" "/account" [sessionCookie visitor] "" `shouldReturn` status401
        stale <- form app "/profile" member [("__csrf_token", csrfToken visitor), ("displayName", "forged")]
        Wai.simpleStatus stale `shouldBe` status403
      it "escapes display names in HTML text and attribute contexts" $ \app -> do
        (_, member) <- openSession app >>= signIn app
        let name = "<script>alert(1)</script>\"&" :: T.Text
        res <- form app "/profile" member [tokenField member, ("displayName", T.encodeUtf8 name)]
        Wai.simpleStatus res `shouldBe` status303
        account <- send app "GET" "/account" [sessionCookie member] ""
        let markup = BL.toStrict $ Wai.simpleBody account
        markup `shouldSatisfy` BS.isInfixOf "&lt;script&gt;"
        markup `shouldSatisfy` BS.isInfixOf "&quot;&amp;"
        markup `shouldSatisfy` (not . BS.isInfixOf "<script>")
      it "revokes logout cookies without replacing them and preserves other sessions" $ \app -> do
        (_, member) <- openSession app >>= signIn app
        (_, other) <- openSession app >>= signIn app
        loggedOut <- form app "/logout" member [tokenField member]
        Wai.simpleStatus loggedOut `shouldBe` status303
        expired <- responseCookie loggedOut
        Cookie.setCookieMaxAge expired `shouldBe` Just 0
        length (filter ((== "Set-Cookie") . fst) $ Wai.simpleHeaders loggedOut) `shouldBe` 1
        statusFor app "GET" "/account" [sessionCookie member] "" `shouldReturn` status401
        statusFor app "GET" "/account" [sessionCookie other] "" `shouldReturn` status200
      it "does not log out through GET or an unprotected POST" $ \app -> do
        (_, member) <- openSession app >>= signIn app
        statusFor app "GET" "/logout" [sessionCookie member] "" `shouldReturn` status404
        res <- form app "/logout" member []
        Wai.simpleStatus res `shouldBe` status403
        statusFor app "GET" "/account" [sessionCookie member] "" `shouldReturn` status200
      forM_ ["/api/profile", "/typed/profile"] $ \path ->
        describe (show path) $ do
          it "requires authentication even with a valid CSRF token" $ \app -> do
            visitor <- openSession app
            statusFor app "POST" path (jsonHeaders visitor) "\"name\"" `shouldReturn` status401
          it "rejects missing or incorrect header tokens before changing state" $ \app -> do
            (_, member) <- openSession app >>= signIn app
            forM_ [[], [("X-Csrf-Token", "wrong")]] $ \tokenHeaders ->
              statusFor app "POST" path (tokenHeaders ++ [sessionCookie member, ("Content-Type", "application/json")])
                "\"forged\"" `shouldReturn` status403
            account <- send app "GET" "/account" [sessionCookie member] ""
            BL.toStrict (Wai.simpleBody account) `shouldSatisfy` (not . BS.isInfixOf "forged")
          it "accepts authenticated JSON with its header token and validates input" $ \app -> do
            (_, member) <- openSession app >>= signIn app
            forM_ ["not-json", "{}", encode (T.replicate 81 "x")] $ \payload ->
              statusFor app "POST" path (jsonHeaders member) payload `shouldReturn` status400
            statusFor app "POST" path (jsonHeaders member) (encode $ T.replicate (17 * 1024) "x") `shouldReturn` status413
            res <- send app "POST" path (jsonHeaders member) "\"Alex\""
            Wai.simpleStatus res `shouldBe` status200
            Wai.simpleBody res `shouldBe` "\"Alex\""
  beforeWith (makeApp LocalHTTP) $
    it "requires an explicit local HTTP configuration to drop Secure" $ \app -> do
      res <- send app "GET" "/" [] ""
      session <- responseCookie res
      Cookie.setCookieName session `shouldBe` "spockcookie"
      Cookie.setCookieSecure session `shouldBe` False
      Cookie.setCookieHttpOnly session `shouldBe` True

-- Disposable fixture, unrelated to any real account or credential.
testPassword :: T.Text
testPassword = "security-guide-test-password"

credentials :: [(BS.ByteString, BS.ByteString)]
credentials = [("username", "demo"), ("password", T.encodeUtf8 testPassword)]

tokenField :: Browser -> (BS.ByteString, BS.ByteString)
tokenField visitor = ("__csrf_token", csrfToken visitor)

jsonHeaders :: Browser -> RequestHeaders
jsonHeaders visitor = [sessionCookie visitor, ("X-Csrf-Token", csrfToken visitor), ("Content-Type", "application/json")]

openSession :: Wai.Application -> IO Browser
openSession app = do
  res <- send app "GET" "/csrf" [] ""
  session <- responseCookie res
  pure $ Browser (cookieHeader session) (BL.toStrict $ Wai.simpleBody res)

signIn :: Wai.Application -> Browser -> IO (Wai.SResponse, Browser)
signIn app visitor = do
  res <- form app "/login" visitor $ tokenField visitor : credentials
  session <- responseCookie res
  token <- send app "GET" "/csrf" [cookieHeader session] ""
  pure (res, Browser (cookieHeader session) (BL.toStrict $ Wai.simpleBody token))

responseCookie :: Wai.SResponse -> IO Cookie.SetCookie
responseCookie res = case lookup "Set-Cookie" (Wai.simpleHeaders res) of
  Nothing -> fail "Expected session cookie"
  Just value -> pure $ Cookie.parseSetCookie value

cookieHeader :: Cookie.SetCookie -> Header
cookieHeader session = ("Cookie", Cookie.setCookieName session <> "=" <> Cookie.setCookieValue session)

form :: Wai.Application -> BS.ByteString -> Browser -> [(BS.ByteString, BS.ByteString)] -> IO Wai.SResponse
form app path visitor fields = send app "POST" path
  [sessionCookie visitor, ("Content-Type", "application/x-www-form-urlencoded")]
  (BL.fromStrict $ renderSimpleQuery False fields)

-- Start each request with a fresh client jar; preserve only the explicitly
-- selected browser cookie, including when exercising another visitor's token.
send :: Wai.Application -> Method -> BS.ByteString -> RequestHeaders -> BL.ByteString -> IO Wai.SResponse
send app method path headers payload = Wai.runSession
  (Wai.srequest $ Wai.SRequest ((Wai.setPath Wai.defaultRequest path)
    { Wai.requestMethod = method, Wai.requestHeaders = headers, Wai.isSecure = True }) payload) app

statusFor :: Wai.Application -> Method -> BS.ByteString -> RequestHeaders -> BL.ByteString -> IO Status
statusFor app method path headers payload = Wai.simpleStatus <$> send app method path headers payload
