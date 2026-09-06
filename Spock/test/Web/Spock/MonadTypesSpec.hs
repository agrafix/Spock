{-# LANGUAGE OverloadedStrings #-}

module Web.Spock.MonadTypesSpec (spec) where

import Control.Monad.Reader (runReaderT)
import Monads (app, coreApp)
import Test.Hspec
import qualified Test.Hspec.Wai as Test
import Web.Spock
import Web.Spock.Config
import qualified Web.Spock.Core as Core

spec :: Spec
spec = describe "Monad layer examples" $ do
  Test.with fullApp $ do
    it "uses shared state during registration and in request helpers" $ do
      Test.get "/setup" `Test.shouldRespondWith` "Hello"
      Test.get "/hello" `Test.shouldRespondWith` "Hello, visitor!"
    it "scopes the typed context to each request and prehook block" $ do
      Test.request "GET" "/hello" [("X-Display-Name", "Alex")] "" `Test.shouldRespondWith` "Hello, Alex!"
      Test.get "/hello" `Test.shouldRespondWith` "Hello, visitor!"
      Test.get "/outside" `Test.shouldRespondWith` "outside the hook"
  Test.with (Core.spockAsApp $ Core.spockT (flip runReaderT "custom base") coreApp) $
    it "lifts into a custom base monad in core Spock" $
      Test.get "/" `Test.shouldRespondWith` "custom base"
  where
    fullApp = do
      cfg <- defaultSpockCfg () PCNoDatabase "Hello"
      spockAsApp $ spock cfg app
