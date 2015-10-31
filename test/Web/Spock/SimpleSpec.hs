{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Spock.SimpleSpec (spec) where

import Web.Spock.Simple
import Web.Spock.FrameworkSpecHelper

import Control.Monad
import Data.Monoid
import Network.HTTP.Types.Status
import Test.Hspec
import qualified Data.Text as T

app :: SpockT IO ()
app =
    do get "/" $ text "root"
       get "/verb-test" $ text "GET"
       post "/verb-test" $ text "POST"
       getpost "/verb-test-gp" $ text "GETPOST"
       put "/verb-test" $ text "PUT"
       delete "/verb-test" $ text "DELETE"
       patch "/verb-test" $ text "PATCH"
       get "test-slash" $ text "ok"
       get "/test-noslash" $ text "ok"
       get "/param-test/:int" $
           do Just (i :: Int) <- param "int"
              text $ "int" <> T.pack (show i)
       get "/param-test/static" $
           text "static"
       subcomponent "/subcomponent" $
         do get "foo" $ text "foo"
            subcomponent "/subcomponent2" $
              get "bar" $ text "bar"
       get "/preferred-format" $
         do fmt <- preferredFormat
            case fmt of
              PrefHTML -> text "html"
              x -> text (T.pack (show x))
       get "/cookie/single" $
           do setCookie "single" "test" defaultCookieSettings { cs_EOL = CookieValidFor 3600 }
              text "set"
       get "/cookie/multiple" $
           do setCookie "multiple1" "test1" defaultCookieSettings { cs_EOL = CookieValidFor 3600 }
              setCookie "multiple2" "test2" defaultCookieSettings { cs_EOL = CookieValidFor 3600 }
              text "set"
       get "set-header" $
           do setHeader "X-FooBar" "Baz"
              text "ok"
       get "set-multi-header" $
           do setHeader "Content-Language" "de"
              setHeader "Content-Language" "en"
              text "ok"
       get ("/auth/:user/:pass") $
           do user <- param' "user"
              pass <- param' "pass"
              let checker user' pass' =
                   unless (user == user' && pass == pass') $
                   do setStatus status401
                      text "err"
              requireBasicAuth "Foo" checker $ \() -> text "ok"
       hookAny GET $ text . T.intercalate "/"

spec :: Spec
spec =
    describe "SimpleRouting" $
    do frameworkSpec (spockAsApp $ spockT id app)
       sizeLimitSpec $ \lim -> spockAsApp $ spockLimT (Just lim) id $
          post "/size" $ body >>= bytes
