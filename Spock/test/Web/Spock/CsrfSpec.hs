{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Web.Spock.CsrfSpec (spec) where

import Web.Spock.TestUtils

import Web.Spock
import Web.Spock.Config

import Data.Monoid
import Test.Hspec
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as T
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Test hiding (request)
import qualified Test.Hspec.Wai as Test

spec :: Spec
spec =
    describe "Csrf" $
    do Test.with testApp $ it "should not require tokens on GET requests" $
          Test.get "/no-token" `Test.shouldRespondWith` "ok" { Test.matchStatus = 200 }
       Test.with testApp $ it "should require tokens on POST requests" $
          Test.post "/token" "" `Test.shouldRespondWith` 403
       Test.with testApp $ it "should requect invalid tokens on POST requests" $
          do Test.post "/token" "__csrf_token=foobarbaz" `Test.shouldRespondWith` 403
             Test.request "POST" "/token" [("X-Csrf-Token", "abc")] ""
                 `Test.shouldRespondWith` 403
       Test.with testApp $ it "should accept valid tokens" $
          do resp <- Test.get "/my-token"
             let Just sessCookie = getSessCookie resp
                 tok = Test.simpleBody resp
                 cc =
                     [ ("Cookie", T.encodeUtf8 $ "spockcookie=" <> sessCookie)
                     , ("X-Csrf-Token", BSL.toStrict tok)
                     ]
                 postCC =
                     ( ("Content-Type", "application/x-www-form-urlencoded")
                     : cc
                     )
                 postPayload =
                     "__csrf_token=" <> tok
             Test.request "POST" "/token" cc "" `Test.shouldRespondWith` 200
             Test.request "POST" "/token" postCC postPayload `Test.shouldRespondWith` 200

testApp :: IO Wai.Application
testApp =
    spockAsApp $
    spockCfg >>= \cfg ->
    spock cfg $
    do get "no-token" $ text "ok"
       get "my-token" $ getCsrfToken >>= text
       post "token" $ text "ok"

spockCfg :: IO (SpockCfg () Int Bool)
spockCfg =
    do cfg <- defaultSpockCfg (0 :: Int) PCNoDatabase True
       return cfg
             { spc_csrfProtection = True
             }
