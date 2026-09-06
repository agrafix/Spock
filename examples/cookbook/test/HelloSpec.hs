{-# LANGUAGE OverloadedStrings #-}

module HelloSpec (spec) where

import Hello (app)
import Test.Hspec
import Test.Hspec.Wai
import Web.Spock (spockAsApp)

spec :: Spec
spec = with (spockAsApp app) $ do
  describe "GET /" $
    it "serves the home page" $
      get "/" `shouldRespondWith` "Hello World!" { matchStatus = 200 }
  describe "GET /hello/:name" $ do
    it "greets the captured name" $
      get "/hello/Spock" `shouldRespondWith` "Hello Spock"
    it "returns 404 for an unmatched route" $
      get "/missing" `shouldRespondWith` 404
