{-# LANGUAGE OverloadedStrings #-}
module Web.Spock.Internal.CookiesSpec (spec) where

import Web.Spock.Internal.Cookies

import Data.Time
import Test.Hspec
import qualified Data.ByteString as BS

spec :: Spec
spec =
    do describe "Generating Cookies" $
        do describe "with the default settings" $
            do let generated = g "foo" "bar" def

               it "should generate the name-value pair" $
                   generated `shouldContainOnce` "foo=bar"

               it "should not generate a max-age key" $
                   generated `shouldNotContain'` "max-age="

               it "should not generate an expires key" $
                   generated `shouldNotContain'` "expires="

               it "should generate a root path" $
                   generated `shouldContainOnce` "path=/"

               it "should not generate a domain pair" $
                   generated `shouldNotContain'` "domain="

               it "should not generate a httponly key" $
                   generated `shouldNotContain'` "HttpOnly"

               it "should not generate a secure key" $
                   generated `shouldNotContain'` "Secure"

           describe "when setting an expiration time in the future" $
            do let generated = g "foo" "bar" def { cs_EOL = CookieValidUntil (UTCTime (fromGregorian 2016 1 1) 0) }

               it "should set the correct expires key" $
                   generated `shouldContainOnce` "expires=Fri, 01-Jan-2016 00:00:00 UTC"

               it "should set the correct max-age key" $
                   generated `shouldContainOnce` "max-age=10465200"

           describe "when setting an expiration time in the past" $
            do let generated = g "foo" "bar" def { cs_EOL = CookieValidUntil (UTCTime (fromGregorian 1970 1 1) 0) }

               it "should set the correct expires key" $
                   generated `shouldContainOnce` "expires=Thu, 01-Jan-1970 00:00:00 UTC"

               it "should set the max-age key to 0" $
                   generated `shouldContainOnce` "max-age=0"

           describe "when setting the path" $
               it "should generate the correct path pair" $
                   g "foo" "bar" def { cs_path = Just "/the-path" } `shouldContainOnce` "path=/the-path"

           describe "when setting the domain" $
               it "should generate the correct domain pair" $
                   g "foo" "bar" def { cs_domain = Just "example.org" } `shouldContainOnce` "domain=example.org"

           describe "when setting the httponly option" $
               it "should generate the httponly key" $
                   g "foo" "bar" def { cs_HTTPOnly = True } `shouldContainOnce` "HttpOnly"

           describe "when setting the secure option" $
               it "should generate the secure key" $
                   g "foo" "bar" def { cs_secure = True } `shouldContainOnce` "Secure"

           describe "cookie value" $
               it "should be urlencoded" $
                   g "foo" "most+special chars;%бисквитки" def `shouldContainOnce`
                     "foo=most%2Bspecial%20chars%3B%25%D0%B1%D0%B8%D1%81%D0%BA%D0%B2%D0%B8%D1%82%D0%BA%D0%B8"

       describe "Parsing cookies" $
           do it "should parse urlencoded multiple cookies" $
                  parseCookies "foo=bar;quux=h&m" `shouldBe` [("foo", "bar"), ("quux", "h&m")]
              it "should handle spacing between cookies" $
                  parseCookies "foo=bar; quux=bim" `shouldBe` [("foo", "bar"), ("quux", "bim")]
              it "should parse urlencoded values" $
                 parseCookies "foo=most%2Bspecial%20chars%3B%25" `shouldBe` [("foo", "most+special chars;%")]

              it "should parse urlencoded utf-8 content" $
                 parseCookies "foo=%D0%B1%D0%B8%D1%81%D0%BA%D0%B2%D0%B8%D1%82%D0%BA%D0%B8" `shouldBe` [("foo", "бисквитки")]
    where
      g n v cs = generateCookieHeaderString n v cs t
      def = defaultCookieSettings
      t = UTCTime (fromGregorian 2015 9 1) (21*60*60)
      shouldContainOnce haystack needle =
          snd (BS.breakSubstring needle haystack) `shouldNotBe` BS.empty
      shouldNotContain' haystack needle =
          snd (BS.breakSubstring needle haystack) `shouldBe` BS.empty
