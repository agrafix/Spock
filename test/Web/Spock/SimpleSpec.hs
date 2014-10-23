{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Spock.SimpleSpec (spec) where

import Web.Spock.Simple
import Web.Spock.FrameworkSpecHelper

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
       subcomponent "/subcomponent" $
         do get "foo" $ text "foo"
            subcomponent "/subcomponent2" $
              do get "bar" $ text "bar"
       hookAny GET $ text . T.intercalate "/"

spec :: Spec
spec = describe "SimpleRouting" $ frameworkSpec (spockAsApp $ spockT id app)
