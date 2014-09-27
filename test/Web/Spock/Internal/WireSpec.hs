{-# LANGUAGE OverloadedStrings #-}
module Web.Spock.Internal.WireSpec (spec) where

import Web.Spock.Internal.Wire
import Test.Hspec

spec :: Spec
spec =
    do describe "combineRoute" $
         do it "handles slashes correctly" $
               do ("/" `combineRoute`  "foo") `shouldBe` "/foo"
                  ("" `combineRoute`  "foo") `shouldBe` "/foo"
                  ("/" `combineRoute`  "/foo") `shouldBe` "/foo"
                  ("" `combineRoute`  "/foo") `shouldBe` "/foo"
                  ("/test" `combineRoute`  "foo") `shouldBe` "/test/foo"
                  ("/test" `combineRoute`  "") `shouldBe` "/test"
