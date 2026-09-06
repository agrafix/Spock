{-# LANGUAGE OverloadedStrings #-}

module Web.Spock.FAQSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import qualified Data.Text.Encoding as T
import FAQ (app)
import Network.HTTP.Types.Status (status200, status403)
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai
import Test.Hspec
import qualified Test.Hspec.Wai as Test
import Web.Spock
import Web.Spock.Config
import Web.Spock.TestUtils (getSessCookie)

spec :: Spec
spec = Test.with application $ describe "FAQ examples" $ do
  it "uses the handler type to parse a capture" $ do
    Test.get "/number/42" `Test.shouldRespondWith` "42"
    Test.get "/number/not-a-number" `Test.shouldRespondWith` 404
  it "matches the entire custom regex capture" $ do
    Test.get "/slug/a-spock-42" `Test.shouldRespondWith` "a-spock-42"
    forM_ ["/slug/UPPER", "/slug/42", "/slug/a%0Ab", "/slug/a%0A", "/slug/a%2Fb", "/slug/a!", "/slug/"] $ \path ->
      Test.get path `Test.shouldRespondWith` 404
  it "captures a wildcard across remaining path segments" $
    Test.get "/rest/a/b/c" `Test.shouldRespondWith` "a/b/c"
  it "renders a form whose token is accepted only with its session" $ \(_, underTest) -> do
    -- Each call has a fresh client jar, so only explicitly supplied cookies
    -- are sent. Network.Wai.Test otherwise prepends its remembered cookie.
    let send method path headers payload = Wai.runSession
          (Wai.srequest $ Wai.SRequest ((Wai.setPath Wai.defaultRequest path)
            { Wai.requestMethod = method, Wai.requestHeaders = headers }) payload) underTest
    response <- send "GET" "/form" [] ""
    let form = T.decodeUtf8 $ BL.toStrict $ Wai.simpleBody response
    case getSessCookie response of
      Nothing -> expectationFailure "Missing form session cookie"
      Just sid -> do
        let cookies = [("Cookie", T.encodeUtf8 $ "spockcookie=" <> sid)]
        tokenResponse <- send "GET" "/csrf" cookies ""
        let token = Wai.simpleBody tokenResponse
        form `shouldContainText` ("value=\"" <> T.decodeUtf8 (BL.toStrict token) <> "\"")
        let payload = "__csrf_token=" <> token
            formType = ("Content-Type", "application/x-www-form-urlencoded")
        accepted <- send "POST" "/form" (formType : cookies) payload
        Wai.simpleStatus accepted `shouldBe` status200
        Wai.simpleBody accepted `shouldBe` "Accepted"
        rejected <- send "POST" "/form" [formType] payload
        Wai.simpleStatus rejected `shouldBe` status403
  it "requires the CSRF header for cookie-authenticated JSON requests" $ do
    response <- Test.get "/csrf"
    case getSessCookie response of
      Nothing -> Test.liftIO $ expectationFailure "Missing JSON session cookie"
      Just sid -> do
        let cookies = [("Cookie", T.encodeUtf8 $ "spockcookie=" <> sid), ("Content-Type", "application/json")]
            headerToken = ("X-Csrf-Token", BL.toStrict $ Wai.simpleBody response)
        Test.request "POST" "/json" cookies "\"hello\"" `Test.shouldRespondWith` 403
        Test.request "POST" "/json" (headerToken : cookies) "\"hello\"" `Test.shouldRespondWith` "\"hello\""
  where
    application = do
      cfg <- defaultBrowserSpockCfg () PCNoDatabase ()
      spockAsApp $ spock cfg app
    shouldContainText actual expected = actual `shouldSatisfy` Text.isInfixOf expected
