{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Spock.CsrfSpec (spec) where

#if MIN_VERSION_base(4,11,0)
#else
import Data.Monoid
#endif

import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as T
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Test hiding (request)
import Test.Hspec
import qualified Test.Hspec.Wai as Test
import Web.Spock
import Web.Spock.Config
import Web.Spock.TestUtils

spec :: Spec
spec =
  describe "Csrf" $
    do
      Test.with (testApp False) $
        it "should not step in if turned of" $
          Test.post "/token" "" `Test.shouldRespondWith` 200
      Test.with (testApp True) $
        it "should not require tokens on GET requests" $
          Test.get "/no-token" `Test.shouldRespondWith` "ok" {Test.matchStatus = 200}
      Test.with (testApp True) $
        it "should require tokens on POST requests" $
          Test.post "/token" "" `Test.shouldRespondWith` 403
      Test.with (testApp True) $
        it "should requect invalid tokens on POST requests" $
          do
            Test.post "/token" "__csrf_token=foobarbaz" `Test.shouldRespondWith` 403
            Test.request "POST" "/token" [("X-Csrf-Token", "abc")] ""
              `Test.shouldRespondWith` 403
      Test.with (testApp True) $
        it "should accept valid tokens" $
          do
            resp <- Test.get "/my-token"
            let Just sessCookie = getSessCookie resp
                tok = Test.simpleBody resp
                cc =
                  [ ("Cookie", T.encodeUtf8 $ "spockcookie=" <> sessCookie),
                    ("X-Csrf-Token", BSL.toStrict tok)
                  ]
                postCC =
                  ( ("Content-Type", "application/x-www-form-urlencoded") :
                    cc
                  )
                postPayload =
                  "__csrf_token=" <> tok
            Test.request "POST" "/token" cc "" `Test.shouldRespondWith` 200
            Test.request "POST" "/token" postCC postPayload `Test.shouldRespondWith` 200
      forM_
        [ ("all-method route", "/all-methods", ["POST", "PUT", "PATCH", "DELETE", "PROPFIND"]),
          ("all-method fallback", "/fallback", ["POST", "PUT", "PATCH", "DELETE", "PROPFIND"]),
          ("custom-method route", "/custom-method", ["PROPFIND"]),
          ("custom-method fallback", "/fallback", ["NOTIFY"])
        ] $ \(label, path, methods) ->
          describe label $
            do
              Test.with (testApp True) $
                it "rejects unsafe requests without tokens" $
                  forM_ methods $ \method ->
                    Test.request method path [] "" `Test.shouldRespondWith` 403
              Test.with (testApp True) $
                it "rejects invalid tokens" $
                  forM_ methods $ \method ->
                    Test.request method path [("X-Csrf-Token", "invalid")] "" `Test.shouldRespondWith` 403
              Test.with (testApp True) $
                it "accepts the session's valid token" $
                  do
                    res <- Test.get "/my-token"
                    case getSessCookie res of
                      Nothing -> Test.liftIO $ expectationFailure "Missing session cookie"
                      Just sid ->
                        let headers =
                              [ ("Cookie", T.encodeUtf8 $ "spockcookie=" <> sid),
                                ("X-Csrf-Token", BSL.toStrict $ Test.simpleBody res)
                              ]
                         in forM_ methods $ \method ->
                              Test.request method path headers "" `Test.shouldRespondWith` 200
              Test.with (testApp False) $
                it "allows requests when protection is disabled" $
                  forM_ methods $ \method ->
                    Test.request method path [] "" `Test.shouldRespondWith` 200
      forM_ ["/all-methods", "/fallback"] $ \path ->
        Test.with (testApp True) $
          it ("allows safe methods without tokens at " ++ show path) $
            forM_ ["GET", "HEAD", "OPTIONS"] $ \method ->
              Test.request method path [] "" `Test.shouldRespondWith` 200

testApp :: Bool -> IO Wai.Application
testApp protect =
  spockAsApp $
    spockCfg protect >>= \cfg ->
      spock cfg $
        do
          get "no-token" $ text "ok"
          get "my-token" $ getCsrfToken >>= text
          post "token" $ text "ok"
          hookRouteAll "all-methods" $ text "ok"
          hookRouteCustom "PROPFIND" "custom-method" $ text "ok"
          hookAnyCustom "NOTIFY" $ \_ -> text "ok"
          hookAnyAll $ \_ -> text "ok"

spockCfg :: Bool -> IO (SpockCfg () Int Bool)
spockCfg protect =
  do
    cfg <- defaultSpockCfg (0 :: Int) PCNoDatabase True
    return
      cfg
        { spc_csrfProtection = protect
        }
