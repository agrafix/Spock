{-# LANGUAGE OverloadedStrings #-}

module Web.Spock.RequestSizeSpec (spec) where

import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as WaiTest
import Test.Hspec
import Web.Spock.Core

spec :: Spec
spec = describe "Lazy request size limits" $
  forM_ [("known length", Wai.KnownLength), ("chunked", const Wai.ChunkedBody)] $ \(label, bodyLength) ->
    describe label $ do
      forM_ consumers $ \(name, contentType, content, consume) ->
        describe name $ do
          it "accepts a body exactly at the limit" $ do
            (res, errors, _) <- send bodyLength contentType content (BS.length content) consume
            WaiTest.simpleStatus res `shouldBe` status200
            WaiTest.simpleBody res `shouldBe` "ok"
            errors `shouldBe` 0
          it "runs the custom 413 handler exactly once when the body exceeds the limit" $ do
            (res, errors, _) <- send bodyLength contentType content (BS.length content - 1) consume
            WaiTest.simpleStatus res `shouldBe` status413
            WaiTest.simpleBody res `shouldBe` "payload too large"
            lookup "X-Size-Error" (WaiTest.simpleHeaders res) `shouldBe` Just "custom"
            errors `shouldBe` 1
      it "does not read an unused body, even when its declared size exceeds the limit" $ do
        (res, errors, reads) <- send bodyLength "text/plain" "unused body" 1 (pure ())
        WaiTest.simpleStatus res `shouldBe` status200
        WaiTest.simpleBody res `shouldBe` "ok"
        errors `shouldBe` 0
        reads `shouldBe` 0
  where
    consumers :: [(String, BS.ByteString, BS.ByteString, ActionT IO ())]
    consumers =
      [ ("raw body", "text/plain", "abcdef", void body),
        ("JSON", "application/json", "{\"value\":42}", void (jsonBody' :: ActionT IO Value)),
        ("form", "application/x-www-form-urlencoded", "value=abcdef", void paramsPost),
        ("multipart", "multipart/form-data; boundary=boundary", multipart, void filesMulti)
      ]
    multipart = "--boundary\r\nContent-Disposition: form-data; name=\"file\"; filename=\"a.txt\"\r\nContent-Type: text/plain\r\n\r\nabcdef\r\n--boundary--\r\n"
    send bodyLength contentType content limit consume = do
      errors <- newIORef (0 :: Int)
      reads <- newIORef (0 :: Int)
      let cfg = defaultSpockConfig
            { sc_maxRequestSize = Just (fromIntegral limit),
              sc_errorHandler = \_ -> do
                liftIO $ modifyIORef' errors (+ 1)
                setHeader "X-Size-Error" "custom"
                text "payload too large"
            }
      app <- spockAsApp $ spockConfigT cfg id $ post root (consume >> text "ok")
      let counted req respond = app req
            { Wai.requestBody = modifyIORef' reads (+ 1) >> Wai.getRequestBodyChunk req } respond
          req = Wai.defaultRequest
            { Wai.requestMethod = "POST",
              Wai.requestHeaders = [(hContentType, contentType)],
              Wai.requestBodyLength = bodyLength (fromIntegral $ BS.length content)
            }
          chunks = map BS.singleton (BS.unpack content)
      res <- WaiTest.runSession (WaiTest.srequest $ WaiTest.SRequest req (LBS.fromChunks chunks)) counted
      (,,) res <$> readIORef errors <*> readIORef reads
