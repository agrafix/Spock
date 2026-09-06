{-# LANGUAGE OverloadedStrings #-}

module CookbookSpec (spec) where

import Cookbook (makeApp)
import Control.Monad (forM_)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai
import Test.Hspec
import Web.Spock.Logging

spec :: Spec
spec = before setup $ describe "Request cookbook" $ do
  it "reads headers, sets response metadata, and correlates structured logs" $ \(app, entries) -> do
    res <- send app "GET" "/headers?secret=hidden-query"
      [("X-Client", "example"), ("Cookie", "hidden-cookie"), ("Authorization", "hidden-authorization")] ""
    Wai.simpleStatus res `shouldBe` status200
    lookup "X-Reply" (Wai.simpleHeaders res) `shouldBe` Just "received"
    lookup "Set-Cookie" (Wai.simpleHeaders res) `shouldBe` Nothing
    rid <- maybe (fail "Missing request ID") (pure . T.decodeUtf8) $ lookup "X-Request-Id" (Wai.simpleHeaders res)
    decode (Wai.simpleBody res) `shouldBe` Just (object ["client" .= String "example", "requestId" .= rid])
    events <- readIORef entries
    map (rc_requestId . le_request) events `shouldSatisfy` all (== rid)
    events `shouldSatisfy` any (isAccess 200 . le_event)
    events `shouldSatisfy` any (isMessage . le_event)
    forM_ ["hidden-query", "hidden-cookie", "hidden-authorization"] $ \secret ->
      BL.toStrict (encode events) `shouldSatisfy` (not . BS.isInfixOf secret)
  it "parses JSON and gives malformed or mistyped bodies a JSON 400" $ \(app, _) -> do
    res <- send app "POST" "/json" jsonHeaders (encode ("hello λ" :: T.Text))
    decode (Wai.simpleBody res) `shouldBe` Just (object ["message" .= String "hello λ"])
    forM_ ["not-json", "{}"] $ \payload -> do
      bad <- send app "POST" "/json" jsonHeaders payload
      assertJsonStatus status400 bad
  it "reads forms from the body and rejects duplicate or query-only names" $ \(app, _) -> do
    res <- send app "POST" "/form" formHeaders "name=Alex%2BSpock"
    decode (Wai.simpleBody res) `shouldBe` Just (object ["name" .= String "Alex+Spock"])
    forM_ [("/form", "name=a&name=b"), ("/form?name=query", "")] $ \(path, payload) -> do
      bad <- send app "POST" path formHeaders payload
      assertJsonStatus status400 bad
  it "runs response middleware on unmatched routes" $ \(app, _) -> do
    res <- send app "GET" "/missing" [] ""
    assertJsonStatus status404 res
    lookup "X-Cookbook" (Wai.simpleHeaders res) `shouldBe` Just "Spock"
  it "logs diagnostic context while the error handler returns a generic JSON 500" $ \(app, entries) -> do
    res <- send app "GET" "/failure" [] ""
    assertJsonStatus status500 res
    BL.toStrict (Wai.simpleBody res) `shouldSatisfy` (not . BS.isInfixOf "intentional failure")
    events <- readIORef entries
    events `shouldSatisfy` any (isError . le_event)
    events `shouldSatisfy` any (isAccess 500 . le_event)
  it "permits the configured CORS origin and preflight without credentials" $ \(app, _) -> do
    res <- send app "OPTIONS" "/json"
      [("Origin", "http://localhost:3000"), ("Access-Control-Request-Method", "POST"), ("Access-Control-Request-Headers", "content-type")] ""
    Wai.simpleStatus res `shouldBe` status200
    lookup "Access-Control-Allow-Origin" (Wai.simpleHeaders res) `shouldBe` Just "http://localhost:3000"
    lookup "Access-Control-Allow-Credentials" (Wai.simpleHeaders res) `shouldBe` Nothing
  it "does not grant CORS access to another origin" $ \(app, _) -> do
    res <- send app "GET" "/headers" [("Origin", "https://untrusted.example")] ""
    lookup "Access-Control-Allow-Origin" (Wai.simpleHeaders res) `shouldBe` Nothing
  it "reads every upload under a repeated field without using client file names as paths" $ \(app, _) -> do
    res <- send app "POST" "/upload" [("Content-Type", "multipart/form-data; boundary=cookbook")]
      (part "../escape.txt" "first" <> part "second.txt" "12" <> "--cookbook--\r\n")
    Wai.simpleStatus res `shouldBe` status201
    decode (Wai.simpleBody res) `shouldBe` Just
      [object ["field" .= String "upload", "name" .= String "../escape.txt", "bytes" .= (5 :: Int)],
       object ["field" .= String "upload", "name" .= String "second.txt", "bytes" .= (2 :: Int)]]
  it "rejects an empty upload with a JSON 400" $ \(app, _) -> do
    res <- send app "POST" "/upload" [] ""
    assertJsonStatus status400 res
  it "limits consumed request bodies and returns a JSON 413" $ \(app, _) -> do
    res <- send app "POST" "/json" jsonHeaders $ BL.replicate (1024 * 1024 + 1) 120
    assertJsonStatus status413 res
  where
    setup = do
      entries <- newIORef []
      app <- makeApp $ \event -> atomicModifyIORef' entries $ \events -> (events ++ [event], ())
      pure (app, entries)
    isAccess expected (AccessLog actual _) = expected == actual
    isAccess _ _ = False
    isMessage (MessageLog LogInfo "Read headers" []) = True
    isMessage _ = False
    isError (ErrorLog value) = "intentional failure" `isInfixOf` T.unpack value
    isError _ = False

jsonHeaders, formHeaders :: RequestHeaders
jsonHeaders = [("Content-Type", "application/json")]
formHeaders = [("Content-Type", "application/x-www-form-urlencoded")]

assertJsonStatus :: Status -> Wai.SResponse -> Expectation
assertJsonStatus status res = do
  Wai.simpleStatus res `shouldBe` status
  lookup "Content-Type" (Wai.simpleHeaders res) `shouldSatisfy` maybe False ("application/json" `BS.isPrefixOf`)
  decode (Wai.simpleBody res) `shouldSatisfy` maybe False isObject
  where
    isObject (Object _) = True
    isObject _ = False

part :: BL.ByteString -> BL.ByteString -> BL.ByteString
part name content = "--cookbook\r\nContent-Disposition: form-data; name=\"upload\"; filename=\""
  <> name <> "\"\r\nContent-Type: text/plain\r\n\r\n" <> content <> "\r\n"

send :: Wai.Application -> Method -> BS.ByteString -> RequestHeaders -> BL.ByteString -> IO Wai.SResponse
send app method path headers payload = Wai.runSession
  (Wai.srequest $ Wai.SRequest ((Wai.setPath Wai.defaultRequest path)
    { Wai.requestMethod = method, Wai.requestHeaders = headers }) payload) app
