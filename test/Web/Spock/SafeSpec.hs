{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Web.Spock.SafeSpec (spec) where

import Web.Spock.Safe
import Web.Spock.FrameworkSpecHelper

import Control.Arrow (second)
import Control.Monad
import Data.IORef
import Data.List (find)
import Data.Monoid
import Network.HTTP.Types.Status
import Test.Hspec
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.Wai.Test as Wai
import qualified Test.Hspec.Wai as Test

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
           do setCookie "single" "test" defaultCookieSettings { cs_EOL = CookieValidFor 3600 }
              text "set"
       get ("cookie" <//> "multiple") $
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
       subcomponent "/subcomponent" $
         do get "foo" $ text "foo"
            subcomponent "/subcomponent2" $
              get "bar" $ text "bar"
       get "preferred-format" $
         do fmt <- preferredFormat
            case fmt of
              PrefHTML -> text "html"
              x -> text (T.pack (show x))
       get ("auth" <//> var <//> var) $ \user pass ->
           let checker user' pass' =
                   unless (user == user' && pass == pass') $
                   do setStatus status401
                      text "err"
           in requireBasicAuth "Foo" checker $ \() -> text "ok"
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

ctxApp :: SpockT IO ()
ctxApp =
    prehook hook $
    do get "test" $ getContext >>= text
       post "test" $ getContext >>= text
    where
      hook =
          do sid <- header "X-ApiKey"
             case sid of
               Just s -> return s
               Nothing -> text "Missing ApiKey"

ctxSpec :: Spec
ctxSpec =
    describe "Contexts" $
    Test.with (spockAsApp $ spockT id ctxApp) $
    it "should work" $
       do Test.request "GET" "/test" [] "" `Test.shouldRespondWith` "Missing ApiKey"
          Test.request "GET" "/test" [("X-ApiKey", "foo")] "" `Test.shouldRespondWith` "foo"
          Test.request "POST" "/test" [("X-ApiKey", "foo")] "" `Test.shouldRespondWith` "foo"

sessionSpec :: Spec
sessionSpec =
    describe "SessionManager" $
    Test.with sessionApp $
    do it "should generate random session ids" $
          do ids <- Test.liftIO $ newIORef HS.empty
             -- of course this sample is too small to prove anything, but it's a good
             -- smoke test...
             replicateM_ 500 $
                do res <- Test.get "/test"
                   Test.liftIO $ checkCookie ids res `shouldReturn` True
       it "should remember a session" $
          do res <- Test.get "/set/5"
             case getSessCookie res of
               Nothing ->
                   Test.liftIO $ expectationFailure "Missing spockcookie"
               Just sessCookie ->
                   Test.request "GET" "/check" [("Cookie", T.encodeUtf8 sessCookie)] ""
                           `Test.shouldRespondWith` "5"
       it "should regenerate and preserve all content" $
          do res <- Test.get "/set/5"
             case getSessCookie res of
               Nothing ->
                   Test.liftIO $ expectationFailure "Missing spockcookie"
               Just sessCookie ->
                   do res2 <- Test.request "GET" "/regenerate" [("Cookie", T.encodeUtf8 sessCookie)] ""
                      case getSessCookie res2 of
                        Nothing ->
                            Test.liftIO $ expectationFailure "Missing new spockcookie"
                        Just sessCookie2 ->
                            do Test.request "GET" "/check" [("Cookie", T.encodeUtf8 sessCookie2)] ""
                                       `Test.shouldRespondWith` "5"
                               Test.request "GET" "/check" [("Cookie", T.encodeUtf8 sessCookie)] ""
                                       `Test.shouldRespondWith` "0"
    where
      sessionApp =
          spockAsApp $
          spock spockCfg $
          do get "test" $ text "text"
             get ("set" <//> var) $ \number -> writeSession number >> text "done"
             get "regenerate" $ sessionRegenerateId >> text "done"
             get "check" $
                 do val <- readSession
                    text (T.pack $ show val)
      spockCfg =
          defaultSpockCfg (0 :: Int) PCNoDatabase True

      getSessCookie :: Wai.SResponse -> Maybe T.Text
      getSessCookie resp =
          let headers = Wai.simpleHeaders resp
          in fmap snd $ find (\h -> fst h == "Set-Cookie" && "spockcookie" `T.isPrefixOf` snd h) $
             map (second T.decodeUtf8) headers

      checkCookie :: IORef (HS.HashSet T.Text) -> Wai.SResponse -> IO Bool
      checkCookie setRef resp =
          do let mSessCookie = getSessCookie resp
             case mSessCookie of
               Nothing ->
                   do expectationFailure "Missing spockcookie"
                      return False
               Just sessCookie ->
                   let (_, cval) = T.breakOn "=" $ fst $ T.breakOn ";" sessCookie
                   in do set <- readIORef setRef
                         if HS.member cval set
                         then return False
                         else do writeIORef setRef $ HS.insert cval set
                                 return True

spec :: Spec
spec =
    describe "SafeRouting" $
    do frameworkSpec (spockAsApp $ spockT id app)
       ctxSpec
       sessionSpec
       routeRenderingSpec
       sizeLimitSpec $ \lim -> spockAsApp $ spockLimT (Just lim) id $
          post "size" $ body >>= bytes
