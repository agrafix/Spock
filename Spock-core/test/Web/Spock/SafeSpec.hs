{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Spock.SafeSpec (spec) where

#if MIN_VERSION_base(4,11,0)
#else
import Data.Monoid
#endif

import qualified Control.Exception as Exception
import Control.Exception.Base
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.HashMap.Strict as HM
import GHC.Generics
import Network.HTTP.Types.Status
import qualified Network.Wai as Wai
import Test.Hspec
import qualified Test.Hspec.Wai as Test
import Web.Spock.Core
import Web.Spock.FrameworkSpecHelper

data SampleJson = SampleJson
  { sampleJson :: T.Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON SampleJson

instance FromJSON SampleJson

app :: SpockT IO ()
app =
  do
    get root $ text "root"
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
      do
        setCookie "single" "test" defaultCookieSettings {cs_EOL = CookieValidFor 3600}
        text "set"
    get ("cookie" <//> "multiple") $
      do
        setCookie "multiple1" "test1" defaultCookieSettings {cs_EOL = CookieValidFor 3600}
        setCookie "multiple2" "test2" defaultCookieSettings {cs_EOL = CookieValidFor 3600}
        text "set"
    get "set-header" $
      do
        setHeader "X-FooBar" "Baz"
        text "ok"
    get "set-multi-header" $
      do
        setHeader "Content-Language" "de"
        setHeader "Content-Language" "en"
        text "ok"
    get "get-params" $
      do
        gp <- paramsGet
        text (T.pack $ show gp)
    post "post-params" $
      do
        gp <- paramsPost
        text (T.pack $ show gp)
    post "json" $
      do
        jbody <- jsonBody'
        text (sampleJson jbody)
    post "raw-body" $
      do
        b <- body
        text (T.decodeUtf8 b)
    let subcomp x =
          "subcomponent" <//> x
        subsubcomp x =
          subcomp ("subcomponent2" <//> x)
    get (subcomp "foo") $ text "foo"
    get (subsubcomp "bar") $ text "bar"
    get "preferred-format" $
      do
        fmt <- preferredFormat
        case fmt of
          PrefHTML -> text "html"
          x -> text (T.pack (show x))
    get ("auth" <//> var <//> var) $ \user pass ->
      let checker user' pass' =
            unless (user == user' && pass == pass') $
              do
                setStatus status401
                text "err"
       in requireBasicAuth "Foo" checker $ \() -> text "ok"
    hookRouteAll ("all" <//> "verbs") $ text "ok"
    hookRouteCustom "NOTIFY" ("notify" <//> var) $ \notification -> text notification
    hookAny GET $ text . T.intercalate "/"
    hookAnyCustom "MYVERB" $ text . T.intercalate "/"
    get ("wai" <//> wildcard) $ \_ ->
      respondApp dummyWai
    post "file/upload" $
      do
        f <- files
        text (T.pack $ show $ HM.size f)
    post "file/upload/multi" $
      do
        f <- files
        let uploadFiles = f HM.! "file"
        text (T.pack $ show $ length $ uploadFiles)

dummyWai :: Wai.Application
dummyWai req respond =
  respond $ Wai.responseLBS status200 [] (BSLC.pack $ show $ Wai.pathInfo req)

routeRenderingSpec :: Spec
routeRenderingSpec =
  describe "Route Rendering" $
    do
      it "should work with argument-less routes" $
        do
          renderRoute "foo" `shouldBe` "/foo"
          renderRoute "/foo" `shouldBe` "/foo"
          renderRoute "/foo/" `shouldBe` "/foo"
          renderRoute ("foo" <//> "bar") `shouldBe` "/foo/bar"
      it "should work with routes with args" $
        do
          let r1 = var :: Var Int
          renderRoute r1 1 `shouldBe` "/1"
          let r2 = "blog" <//> (var :: Var Int)
          renderRoute r2 2 `shouldBe` "/blog/2"
          let r3 = "blog" <//> (var :: Var Int) <//> (var :: Var T.Text)
          renderRoute r3 2 "BIIM" `shouldBe` "/blog/2/BIIM"

data InstancesTestException = InstancesTestException deriving (Show)

instance Exception InstancesTestException

instancesApp :: SpockCtxT () (ReaderT T.Text IO) ()
instancesApp =
  do
    get ("instances" <//> "monad-base") $ text =<< liftBase (pure "ok")
    get ("instances" <//> "monad-base-control") $
      do
        res <-
          catch'
            (throwIO' InstancesTestException)
            (\(_ :: InstancesTestException) -> pure "ok")
        text res
    get ("instances" <//> "monad-trans-control") $
      do
        res <- liftWith (\run -> ask >>= run . text)
        restoreT $ pure res
  where
    -- This is 'catch' from 'Control.Exception.Lifted'.
    catch' :: (MonadBaseControl IO m, Exception e) => m a -> (e -> m a) -> m a
    catch' a handler =
      control $ \runInIO ->
        Exception.catch (runInIO a) (\e -> runInIO $ handler e)

    -- This is 'throwIO' from 'Control.Exception.Lifted'.
    throwIO' :: (MonadBase IO m, Exception e) => e -> m a
    throwIO' = liftBase . Exception.throwIO

instancesSpec :: Spec
instancesSpec =
  describe "Instances for ActionT are correct" $
    Test.with (spockAsApp $ spockT (flip runReaderT "ok") instancesApp) $
      do
        it "MonadBase" $
          Test.request "GET" "/instances/monad-base" [] "" `Test.shouldRespondWith` "ok"
        it "MonadBaseControl" $
          Test.request "GET" "/instances/monad-base-control" [] "" `Test.shouldRespondWith` "ok"
        it "MonadTransControl" $
          Test.request "GET" "/instances/monad-trans-control" [] "" `Test.shouldRespondWith` "ok"

ctxApp :: SpockT IO ()
ctxApp =
  prehook hook $
    do
      get "test" $ getContext >>= text
      post "test" $ getContext >>= text
  where
    hook =
      do
        sid <- header "X-ApiKey"
        case sid of
          Just s -> return s
          Nothing -> text "Missing ApiKey"

ctxSpec :: Spec
ctxSpec =
  describe "Contexts" $
    Test.with (spockAsApp $ spockT id ctxApp) $
      it "should work" $
        do
          Test.request "GET" "/test" [] "" `Test.shouldRespondWith` "Missing ApiKey"
          Test.request "GET" "/test" [("X-ApiKey", "foo")] "" `Test.shouldRespondWith` "foo"
          Test.request "POST" "/test" [("X-ApiKey", "foo")] "" `Test.shouldRespondWith` "foo"

spec :: Spec
spec =
  describe "SafeRouting" $
    do
      frameworkSpec (spockAsApp $ spockT id app)
      ctxSpec
      instancesSpec
      routeRenderingSpec
      sizeLimitSpec $ \lim ->
        spockAsApp $
          spockConfigT (defaultSpockConfig {sc_maxRequestSize = Just lim}) id $
            post "size" $ body >>= bytes
      errorHandlerSpec $
        spockAsApp $
          spockConfigT specConfig id $ do
            get ("failing" <//> "route") $
              throw Overflow
            get ("user" <//> "error") $
              do
                setStatus status403
                text "UNAUTHORIZED"
  where
    specConfig = defaultSpockConfig {sc_errorHandler = errorHandler}
    errorHandler status = case statusCode status of
      500 -> text "SERVER ERROR"
      404 -> text "NOT FOUND"
      _ -> text "OTHER ERROR"
