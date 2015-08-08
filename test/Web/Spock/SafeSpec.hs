{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Spock.SafeSpec (spec) where

import Web.Spock.Safe
import Web.Spock.FrameworkSpecHelper

import Data.Monoid
import Test.Hspec
import qualified Data.Text as T

app :: SpockT IO ()
app =
    do get root $ text "root"
       get "verb-test" $ text "GET"
       post "verb-test" $ text "POST"
       getpost "verb-test-gp" $ text "GETPOST"
       put "verb-test" $ text "PUT"
       delete "verb-test" $ text "DELETE"
       patch "verb-test" $ text "PATCH"
       get "test-slash" $ text "ok"
       get "/test-noslash" $ text "ok"
       get ("param-test" <//> var) $ \(i :: Int) ->
           text $ "int" <> T.pack (show i)
       get ("param-test" <//> "static") $
           text "static"
       get ("cookie" <//> "single") $
           do setCookie "single" "test" 3600
              text "set"
       get ("cookie" <//> "multiple") $
           do setCookie "multiple1" "test1" 3600
              setCookie "multiple2" "test2" 3600
              text "set"
       subcomponent "/subcomponent" $
         do get "foo" $ text "foo"
            subcomponent "/subcomponent2" $
              get "bar" $ text "bar"
       get "preferred-format" $
         do fmt <- preferredFormat
            case fmt of
              PrefHTML -> text "html"
              x -> text (T.pack (show x))
       hookAny GET $ text . T.intercalate "/"

routeRenderingSpec :: Spec
routeRenderingSpec =
    describe "Route Rendering" $
    do it "should work with argument-less routes" $
          do renderRoute "foo" `shouldBe` "/foo"
             renderRoute "/foo" `shouldBe` "/foo"
             renderRoute "/foo/" `shouldBe` "/foo"
             renderRoute ("foo" <//> "bar") `shouldBe` "/foo/bar"
       it "should work with routes with args" $
          do let r1 = var :: Var Int
             renderRoute r1 1 `shouldBe` "/1"
             let r2 = "blog" <//> (var :: Var Int)
             renderRoute r2 2 `shouldBe` "/blog/2"
             let r3 = "blog" <//> (var :: Var Int) <//> (var :: Var T.Text)
             renderRoute r3 2 "BIIM" `shouldBe` "/blog/2/BIIM"

spec :: Spec
spec =
    describe "SafeRouting" $
    do frameworkSpec (spockAsApp $ spockT id app)
       routeRenderingSpec
       sizeLimitSpec $ \lim -> spockAsApp $ spockLimT (Just lim) id $
          post "size" $ body >>= bytes
