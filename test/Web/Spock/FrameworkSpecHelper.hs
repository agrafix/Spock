{-# LANGUAGE OverloadedStrings #-}
module Web.Spock.FrameworkSpecHelper where

import Test.Hspec
import Test.Hspec.Wai

import Data.Monoid
import Data.Word
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.Wai as Wai

sizeLimitSpec :: (Word64 -> IO Wai.Application) -> Spec
sizeLimitSpec app =
    with (app maxSize) $
    describe "Request size limit" $
    do it "allows small enough requests the way" $
          do post "/size" okBs `shouldRespondWith` matcher 200 okBs
             post "/size" okBs2 `shouldRespondWith` matcher 200 okBs2
       it "denys large requests the way" $
          post "/size" tooLongBs `shouldRespondWith` 413
    where
      matcher s b =
          ResponseMatcher
          { matchStatus = s
          , matchBody = Just b
          , matchHeaders = []
          }
      maxSize = 1024
      okBs = BSLC.replicate (fromIntegral maxSize - 50) 'i'
      okBs2 = BSLC.replicate (fromIntegral maxSize) 'j'
      tooLongBs = BSLC.replicate (fromIntegral maxSize + 100) 'k'


frameworkSpec :: IO Wai.Application -> Spec
frameworkSpec app =
    with app $
    do routingSpec
       actionSpec
       headerTest
       cookieTest

routingSpec :: SpecWith Wai.Application
routingSpec =
    describe "Routing Framework" $
      do it "allows root actions" $
            get "/" `shouldRespondWith` "root" { matchStatus = 200 }
         it "routes different HTTP-verbs to different actions" $
            do verbTest get "GET"
               verbTest (`post` "") "POST"
               verbTest (`put` "") "PUT"
               verbTest delete "DELETE"
               verbTest (`patch` "") "PATCH"
               verbTestGp get "GETPOST"
               verbTestGp (`post` "") "GETPOST"
         it "can extract params from routes" $
            get "/param-test/42" `shouldRespondWith` "int42" { matchStatus = 200 }
         it "can handle multiple matching routes" $
            get "/param-test/static" `shouldRespondWith` "static" { matchStatus = 200 }
         it "ignores trailing slashes" $
            get "/param-test/static/" `shouldRespondWith` "static" { matchStatus = 200 }
         it "works with subcomponents" $
            do get "/subcomponent/foo" `shouldRespondWith` "foo" { matchStatus = 200 }
               get "/subcomponent/subcomponent2/bar" `shouldRespondWith` "bar" { matchStatus = 200 }
         it "allows the definition of a fallback handler" $
            get "/askldjas/aklsdj" `shouldRespondWith` "askldjas/aklsdj" { matchStatus = 200 }
         it "detected the preferred format" $
            request "GET" "/preferred-format" [("Accept", "text/html,application/xml;q=0.9,image/webp,*/*;q=0.8")] "" `shouldRespondWith` "html" { matchStatus = 200 }
         it "/test-slash and test-noslash are the same thing" $
            do get "/test-slash" `shouldRespondWith` "ok" { matchStatus = 200 }
               get "test-slash" `shouldRespondWith` "ok" { matchStatus = 200 }
               get "/test-noslash" `shouldRespondWith` "ok" { matchStatus = 200 }
               get "test-noslash" `shouldRespondWith` "ok" { matchStatus = 200 }
    where
      verbTestGp verb verbVerbose =
          verb "/verb-test-gp" `shouldRespondWith` (verbVerbose { matchStatus = 200 })
      verbTest verb verbVerbose =
          verb "/verb-test" `shouldRespondWith` (verbVerbose { matchStatus = 200 })

actionSpec :: SpecWith Wai.Application
actionSpec =
    describe "Action Framework" $ return ()

cookieTest :: SpecWith Wai.Application
cookieTest =
    describe "Cookies" $
    do it "sets single cookies correctly" $
          get "/cookie/single" `shouldRespondWith`
                  "set"
                  { matchStatus = 200
                  , matchHeaders =
                      [ matchCookie "single" "test"
                      ]
                  }
       it "sets multiple cookies correctly" $
          get "/cookie/multiple" `shouldRespondWith`
                  "set"
                  { matchStatus = 200
                  , matchHeaders =
                      [ matchCookie "multiple1" "test1"
                      , matchCookie "multiple2" "test2"
                      ]
                  }
headerTest :: SpecWith Wai.Application
headerTest =
    describe "Headers" $
    do it "supports custom headers" $
          get "/set-header" `shouldRespondWith`
                  "ok"
                  { matchStatus = 200
                  , matchHeaders =
                      [ "X-FooBar" <:> "Baz"
                      ]
                  }
       it "supports multi headers" $
          get "/set-multi-header" `shouldRespondWith`
                  "ok"
                  { matchStatus = 200
                  , matchHeaders =
                      [ "Content-Language" <:> "de"
                      , "Content-Language" <:> "en"
                      ]
                  }

matchCookie :: T.Text -> T.Text -> MatchHeader
matchCookie name val =
    MatchHeader $ \headers ->
        let relevantHeaders = filter (\h -> fst h == "Set-Cookie") headers
            loop [] =
                Just ("No cookie named " ++ T.unpack name ++ " with value "
                      ++ T.unpack val ++ " found")
            loop (x:xs) =
                let (cname, cval) = T.breakOn "=" $ fst $ T.breakOn ";" $ T.decodeUtf8 $ snd x
                in if cname == name && cval == "=" <> val
                   then Nothing
                   else loop xs
        in loop relevantHeaders
