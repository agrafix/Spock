{-# LANGUAGE OverloadedStrings #-}
module Web.Spock.FrameworkSpecHelper where

import Test.Hspec
import Test.Hspec.Wai

import qualified Network.Wai as Wai

frameworkSpec :: IO Wai.Application -> Spec
frameworkSpec app =
    with app $
    do routingSpec
       actionSpec

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
