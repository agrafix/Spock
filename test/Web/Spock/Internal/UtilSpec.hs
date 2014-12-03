{-# LANGUAGE OverloadedStrings #-}
module Web.Spock.Internal.UtilSpec (spec) where

import Web.Spock.Internal.Util

import Test.Hspec

spec :: Spec
spec =
     describe "Utils" $
     do describe "detectPreferredFormat" $
          do it "should detect json-only requests" $
               do detectPreferredFormat "application/json, text/javascript, */*; q=0.01" `shouldBe` PrefJSON
                  detectPreferredFormat "application/json;" `shouldBe` PrefJSON
                  detectPreferredFormat "text/javascript;" `shouldBe` PrefJSON
             it "should detect browsers as html clients" $
                do detectPreferredFormat "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8" `shouldBe` PrefHTML
                   detectPreferredFormat "text/html;" `shouldBe` PrefHTML
                   detectPreferredFormat "application/xhtml+xml;" `shouldBe` PrefHTML
             it "should detect xml-only requests" $
                do detectPreferredFormat "application/xml, text/xml, */*; q=0.01" `shouldBe` PrefXML
                   detectPreferredFormat "application/xml;" `shouldBe` PrefXML
                   detectPreferredFormat "text/xml;" `shouldBe` PrefXML
             it "should detect text-only requests" $
                do detectPreferredFormat "text/plain, */*; q=0.01" `shouldBe` PrefText
                   detectPreferredFormat "text/plain;" `shouldBe` PrefText
