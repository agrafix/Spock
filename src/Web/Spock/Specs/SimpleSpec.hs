{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Spock.Specs.SimpleSpec where

import Web.Spock.Simple
import Web.Spock.Specs.FrameworkSpecHelper

import Data.Monoid
import Test.Hspec
import qualified Data.Text as T

app :: SpockT IO ()
app =
    do get "/" $ text "root"
       get "/verb-test" $ text "GET"
       post "/verb-test" $ text "POST"
       put "/verb-test" $ text "PUT"
       delete "/verb-test" $ text "DELETE"
       patch "/verb-test" $ text "PATCH"
       get "/param-test/:int" $
           do Just (i :: Int) <- param "int"
              text $ "int" <> (T.pack $ show i)
       get "/param-test/static" $
           text "static"
spec :: Spec
spec = describe "SimpleRouting" $ frameworkSpec (spockApp id app)
