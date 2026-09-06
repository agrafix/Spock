{-# LANGUAGE OverloadedStrings #-}

module Web.Spock.LoggingSpec (spec) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (AsyncException (ThreadKilled), throwIO)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..), toJSON)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.List (nub)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai
import Test.Hspec
import Web.Spock.Core hiding (head, request)

spec :: Spec
spec = describe "Request logging" $ do
  it "correlates handler, context, response, and access events" $ do
    (app, events, _) <- loggingApp id id
    result <- send app "/ok?secret=hidden" []
    let sid = lookup "X-Request-Id" $ Wai.simpleHeaders result
    sid `shouldSatisfy` maybe False ((== 32) . BS.length)
    Just (LBS.toStrict $ Wai.simpleBody result) `shouldBe` sid
    logged <- readIORef events
    length logged `shouldBe` 2
    map (T.encodeUtf8 . rc_requestId . le_request) logged `shouldBe` maybe [] (replicate 2) sid
    map (rc_path . le_request) logged `shouldBe` ["/ok", "/ok"]
    case map le_event logged of
      [MessageLog LogInfo "served" fields, AccessLog 200 _] -> fields `shouldBe` [("count", Number 3), ("requestId", String "custom-field")]
      other -> expectationFailure $ "Unexpected events: " ++ show other
    case toJSON (head logged) of
      Object object -> do
        KM.lookup "requestId" object `shouldBe` (String . T.decodeUtf8 <$> sid)
        KM.lookup "fields" object `shouldBe` Just (Object $ KM.fromList [("count", Number 3), ("requestId", String "custom-field")])
      value -> expectationFailure $ show value

  it "generates distinct IDs under concurrent requests" $ do
    (app, _, _) <- loggingApp id id
    responses <- mapConcurrently (const $ send app "/ok" []) [1 :: Int .. 40]
    length (nub $ map (lookup "X-Request-Id" . Wai.simpleHeaders) responses) `shouldBe` 40

  it "ignores incoming IDs by default" $ do
    (app, _, _) <- loggingApp id id
    result <- send app "/ok" [("X-Request-Id", "untrusted")]
    lookup "X-Request-Id" (Wai.simpleHeaders result) `shouldNotBe` Just "untrusted"

  it "accepts a valid ID from a trusted proxy" $ do
    (app, _, _) <- loggingApp (\cfg -> cfg { lc_trustIncomingRequestId = True }) id
    result <- send app "/ok" [("X-Request-Id", "edge.1_test-2")]
    Wai.simpleBody result `shouldBe` "edge.1_test-2"

  forM_ [[], [""], ["bad id"], ["bad\r\nheader"], [BS.replicate 129 65], ["one", "two"], ["\xff"]] $ \values ->
    it ("generates a safe replacement for missing or invalid IDs: " ++ show values) $ do
      (app, _, _) <- loggingApp (\cfg -> cfg { lc_trustIncomingRequestId = True, lc_generateRequestId = pure "generated" }) id
      result <- send app "/ok" [("X-Request-Id", value) | value <- values]
      lookup "X-Request-Id" (Wai.simpleHeaders result) `shouldBe` Just "generated"

  it "validates custom generated IDs and replaces unsafe output" $ do
    (app, _, _) <- loggingApp (\cfg -> cfg { lc_generateRequestId = pure "invalid\nheader" }) id
    result <- send app "/ok" []
    lookup "X-Request-Id" (Wai.simpleHeaders result) `shouldSatisfy` maybe False ((== 32) . BS.length)

  it "uses the configured header and replaces conflicting response values" $ do
    events <- newIORef []
    let logger = (defaultLoggingConfig $ record events)
          { lc_requestIdHeader = "X-Correlation-Id", lc_trustIncomingRequestId = True }
    app <- spockAsApp $ spockConfigT (defaultSpockConfig { sc_logging = Just logger }) id $
      get root $ setHeader "X-Correlation-Id" "handler-value" >> text "ok"
    result <- send app "/" [("X-Correlation-Id", "proxy-value")]
    filter ((== "X-Correlation-Id") . fst) (Wai.simpleHeaders result)
      `shouldBe` [("X-Correlation-Id", "proxy-value")]

  it "reports errors with the same ID and preserves legacy hooks" $ do
    (app, events, legacy) <- loggingApp id id
    result <- send app "/boom" []
    Wai.simpleStatus result `shouldBe` status500
    logged <- readIORef events
    length (filter isError logged) `shouldBe` 1
    length (filter isAccess logged) `shouldBe` 1
    length (nub $ map (rc_requestId . le_request) logged) `shouldBe` 1
    length <$> readIORef legacy `shouldReturn` 1

  it "keeps the ID in custom error handlers without logging access twice" $ do
    (app, events, _) <- loggingApp id (\cfg -> cfg { sc_errorHandler = \_ -> do
      logMessage LogWarning "custom error" []
      getRequestId >>= text . maybe "missing" id })
    result <- send app "/missing" []
    Wai.simpleStatus result `shouldBe` status404
    Just (LBS.toStrict $ Wai.simpleBody result) `shouldBe` lookup "X-Request-Id" (Wai.simpleHeaders result)
    logged <- readIORef events
    length (filter isAccess logged) `shouldBe` 1
    length logged `shouldBe` 2

  it "preserves correlation across route fallthrough" $ do
    (app, events, _) <- loggingApp id id
    result <- send app "/next" []
    Wai.simpleStatus result `shouldBe` status200
    logged <- readIORef events
    length logged `shouldBe` 3
    length (nub $ map (rc_requestId . le_request) logged) `shouldBe` 1

  it "includes responses produced by WAI middleware" $ do
    events <- newIORef []
    let config = defaultSpockConfig { sc_logging = Just $ defaultLoggingConfig (record events) }
    app <- spockAsApp $ spockConfigT config id $ middleware $ \_ _ respond -> respond $ Wai.responseLBS status204 [] ""
    result <- send app "/middleware" []
    Wai.simpleStatus result `shouldBe` status204
    logged <- readIORef events
    map le_event logged `shouldSatisfy` (\xs -> case xs of [AccessLog 204 _] -> True; _ -> False)

  it "does not let synchronous sink failures change the response" $ do
    failures <- newIORef (0 :: Int)
    (app, _, _) <- loggingApp (\cfg -> cfg { lc_logEvent = \_ -> ioError $ userError "sink failed",
      lc_logFailure = \_ -> modifyIORef' failures (+ 1) }) id
    result <- send app "/ok" []
    Wai.simpleStatus result `shouldBe` status200
    readIORef failures `shouldReturn` 2

  it "logs uncaught middleware errors and rethrows them" $ do
    events <- newIORef []
    logger <- newRequestLogger $ defaultLoggingConfig (record events)
    let app = requestLoggingMiddleware logger $ \_ _ -> ioError $ userError "middleware failed"
    send app "/broken" [] `shouldThrow` anyIOException
    logged <- readIORef events
    length (filter isError logged) `shouldBe` 1
    length (filter isAccess logged) `shouldBe` 0

  it "propagates asynchronous cancellation from handlers and sinks" $ do
    (app, _, _) <- loggingApp id id
    send app "/cancel" [] `shouldThrow` (== ThreadKilled)
    (sinkApp, _, _) <- loggingApp (\cfg -> cfg { lc_logEvent = \_ -> throwIO ThreadKilled }) id
    send sinkApp "/ok" [] `shouldThrow` (== ThreadKilled)

  it "leaves logging optional" $ do
    (app, events, _) <- loggingApp id (\cfg -> cfg { sc_logging = Nothing })
    result <- send app "/ok" []
    Wai.simpleBody result `shouldBe` "disabled"
    lookup "X-Request-Id" (Wai.simpleHeaders result) `shouldBe` Nothing
    readIORef events `shouldReturn` []

loggingApp :: (LoggingConfig -> LoggingConfig) -> (SpockConfig -> SpockConfig) -> IO (Wai.Application, IORef [LogEvent], IORef [T.Text])
loggingApp loggingChange configChange = do
  events <- newIORef []
  legacy <- newIORef []
  let config = configChange $ defaultSpockConfig
        { sc_logError = record legacy, sc_logging = Just $ loggingChange $ defaultLoggingConfig (record events) }
  app <- spockAsApp $ spockConfigT config id $ do
    get "ok" $ runInContext ("context" :: T.Text) $ do
      logMessage LogInfo "served" [("count", Number 3), ("requestId", String "custom-field")]
      getRequestId >>= text . maybe "disabled" id
    get "boom" $ liftIO $ ioError $ userError "handler failed"
    get "cancel" $ liftIO $ throwIO ThreadKilled
    get "next" $ logMessage LogInfo "second" [] >> text "ok"
    get "next" $ logMessage LogDebug "first" [] >> jumpNext
  pure (app, events, legacy)

record :: IORef [a] -> a -> IO ()
record ref value = atomicModifyIORef' ref $ \values -> (values ++ [value], ())

isAccess :: LogEvent -> Bool
isAccess event = case le_event event of AccessLog _ _ -> True; _ -> False

isError :: LogEvent -> Bool
isError event = case le_event event of ErrorLog _ -> True; _ -> False

send :: Wai.Application -> BS.ByteString -> RequestHeaders -> IO Wai.SResponse
send app path headers = Wai.runSession
  (Wai.srequest $ Wai.SRequest ((Wai.setPath Wai.defaultRequest path) { Wai.requestHeaders = headers }) "") app
