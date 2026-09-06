{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Test
import Rendering (app)
import Test.Hspec
import Web.Spock.Core (spockAsApp, spockT)

main :: IO ()
main = hspec $ before (spockAsApp $ spockT id app) $
  forM_ ["/", "/utf8", "/lucid"] $ \path -> describe (show path) $ do
    it "renders UTF-8 HTML with the correct content type" $ \application -> do
      res <- Test.runSession (Test.request $ Test.setPath Wai.defaultRequest path) application
      Test.simpleStatus res `shouldBe` status200
      lookup hContentType (Test.simpleHeaders res) `shouldBe` Just "text/html; charset=utf-8"
      Test.simpleBody res `shouldBe` LBS.fromStrict (TE.encodeUtf8 "<p>Hello, λ!</p>")
    it "escapes text from the request" $ \application -> do
      res <- Test.runSession (Test.request $ Test.setPath Wai.defaultRequest (path <> "?message=%3Cscript%3E%26%CE%BB")) application
      Test.simpleStatus res `shouldBe` status200
      Test.simpleBody res `shouldBe` LBS.fromStrict (TE.encodeUtf8 "<p>&lt;script&gt;&amp;λ</p>")
