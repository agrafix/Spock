{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Exception (AsyncException (ThreadKilled), throwIO)
import Control.Monad (forM_)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.HVect (HVect (..))
import Data.IORef
import qualified Data.Text as T
import Test.Hspec
import Web.HttpApiData
import Web.Spock.Api
import Web.Spock.Api.Client
import Web.Spock.Api.Document

main :: IO ()
main = hspec $ do
  describe "Typed request encoding" $ do
    forM_ [("GET", MethodGet route), ("DELETE", MethodDelete route)] $ \(method, endpoint) ->
      it ("encodes " ++ T.unpack method ++ " paths without an invented body") $ do
        request <- require $ prepareEndpoint defaultClientConfig (endpoint :: Endpoint '[T.Text] 'Nothing T.Text) [] ("a/b λ?#%" :&: HNil) HNil
        rq_method request `shouldBe` method
        rq_url request `shouldBe` "/api/a%2Fb%20%CE%BB%3F%23%25"
        rq_body request `shouldBe` Nothing
        rq_headers request `shouldBe` []
    forM_ [("POST", MethodPost Proxy route), ("PUT", MethodPut Proxy route), ("PATCH", MethodPatch Proxy route)] $ \(method, endpoint) ->
      it ("encodes " ++ T.unpack method ++ " JSON with the declared method") $ do
        request <- require $ prepareEndpoint defaultClientConfig (endpoint :: Endpoint '[T.Text] ('Just T.Text) T.Text) [] ("id" :&: HNil) ("λ😀" :&: HNil)
        rq_method request `shouldBe` method
        rq_body request `shouldBe` Just (BL.toStrict $ A.encode ("λ😀" :: T.Text))
        rq_headers request `shouldBe` [("Content-Type", "application/json;charset=UTF-8")]
    it "encodes query and header arguments in declaration order" $ do
      request <- require $ prepareDocumentedEndpoint defaultClientConfig documented [] ("λ/file" :&: HNil)
        ("a+b&c" :&: Just 2 :&: ["first", "two words"] :&: "requester" :&: Just "optional" :&: HNil) HNil
      rq_url request `shouldBe` "/api/%CE%BB%2Ffile?search=a%2Bb%26c&offset=2&tag=first&tag=two%20words"
      rq_headers request `shouldBe` [("X-Requested-By", "requester"), ("X-Optional", "optional")]
    it "omits missing optionals and empty repeated queries" $ do
      request <- require $ prepareDocumentedEndpoint defaultClientConfig documented [] ("id" :&: HNil)
        ("" :&: Nothing :&: [] :&: "caller" :&: Nothing :&: HNil) HNil
      rq_url request `shouldBe` "/api/id?search="
      rq_headers request `shouldBe` [("X-Requested-By", "caller")]
    it "uses custom query/header encoders carried by the shared definitions" $ do
      request <- require $ prepareDocumentedEndpoint defaultClientConfig custom [] HNil (Custom :&: Custom :&: HNil) HNil
      rq_url request `shouldBe` "/custom?query=query-value"
      rq_headers request `shouldBe` [("X-Custom", "header-value")]
    it "combines a base URL, typed extension, global headers and per-call CSRF header" $ do
      let cfg = defaultClientConfig { cc_baseUrl = "https://example.test/prefix/", cc_headers = [("X-App", "demo")] }
          endpoint = MethodGet ("files" <//> (var <.> "json")) :: Endpoint '[T.Text] 'Nothing T.Text
      request <- require $ prepareEndpoint cfg endpoint [("X-Csrf-Token", "token")] ("a b" :&: HNil) HNil
      rq_url request `shouldBe` "https://example.test/prefix/files/a%20b.json"
      rq_headers request `shouldBe` [("X-App", "demo"), ("X-Csrf-Token", "token")]
      rq_credentials request `shouldBe` SameOrigin
    it "preserves explicitly configured strict trailing slashes" $ do
      let endpoint = MethodGet (trailingSlash route) :: Endpoint '[T.Text] 'Nothing T.Text
      request <- require $ prepareEndpoint (defaultClientConfig { cc_slashPolicy = StrictSlashes }) endpoint [] ("name" :&: HNil) HNil
      rq_url request `shouldBe` "/api/name/"
    it "rejects duplicate or invalid headers before invoking the transport" $ do
      client <- require $ newClient defaultClientConfig (\_ -> fail "unexpected transport")
      forM_ [[("X-Test", "one"), ("x-test", "two")], [("bad name", "value")], [("X-Test", "line\r\ninjected")]] $ \headers ->
        callEndpoint' client (MethodGet "value" :: Endpoint '[] 'Nothing T.Text) headers `shouldReturn` Left InvalidRequest
    it "rejects duplicate typed/extra headers and invalid endpoint metadata" $ do
      let values = "search" :&: Nothing :&: [] :&: "caller" :&: Nothing :&: HNil
      errorOf (prepareDocumentedEndpoint defaultClientConfig documented [("x-requested-by", "duplicate")] ("id" :&: HNil) values HNil) `shouldBe` Just InvalidRequest
      errorOf (prepareDocumentedEndpoint defaultClientConfig (documented { de_operation = operationInfo "" }) [] ("id" :&: HNil) values HNil) `shouldBe` Just InvalidEndpoint
    it "rejects origin-changing and browser-normalized path segments" $ do
      let endpoint = MethodGet (var <//> "data") :: Endpoint '[T.Text] 'Nothing T.Text
      forM_ ["", ".", ".."] $ \value ->
        errorOf (prepareEndpoint defaultClientConfig endpoint [] (value :&: HNil) HNil) `shouldBe` Just InvalidRequest
    it "validates URL, timeout, response budget and header configuration" $ do
      forM_ ["//evil.test", "relative", "https://user:password@example.test", "ftp://example.test", "/api?token=secret", "/api#fragment", "/\\evil", "/a/../b", "/%2E%2e/api"] $ \url ->
        errorOf (newClient (defaultClientConfig { cc_baseUrl = url }) unused) `shouldBe` Just InvalidClientConfig
      forM_ [0, -1] $ \limit -> do
        errorOf (newClient (defaultClientConfig { cc_timeoutMilliseconds = limit }) unused) `shouldBe` Just InvalidClientConfig
        errorOf (newClient (defaultClientConfig { cc_maxResponseBytes = limit }) unused) `shouldBe` Just InvalidClientConfig
      errorOf (newClient (defaultClientConfig { cc_headers = [("bad", "\0")] }) unused) `shouldBe` Just InvalidClientConfig
  describe "Typed responses and errors" $ do
    it "accepts valid JSON throughout the successful status range" $
      forM_ [200, 201, 202, 299] $ \status -> do
        client <- returning defaultClientConfig $ Right $ Response status "\"value\""
        callEndpoint client textEndpoint `shouldReturn` Right "value"
    it "returns HTTP errors without decoding or reporting the error body" $
      forM_ [301, 400, 403, 404, 500] $ \status -> do
        client <- returning defaultClientConfig $ Right $ Response status "private error body"
        callEndpoint client textEndpoint `shouldReturn` Left (HttpError status)
    it "distinguishes empty, malformed and wrong-schema JSON from network errors" $ do
      forM_ ["", "not-json", "42", B.pack [34,255,34]] $ \body -> do
        client <- returning defaultClientConfig $ Right $ Response 200 body
        callEndpoint client textEndpoint `shouldReturn` Left DecodeFailure
      forM_ [NetworkFailure, RequestTimedOut, ResponseTooLarge] $ \err -> do
        client <- returning defaultClientConfig $ Left err
        callEndpoint client textEndpoint `shouldReturn` Left err
    it "accepts the exact byte limit and rejects the next byte" $ do
      good <- returning (defaultClientConfig { cc_maxResponseBytes = 4 }) $ Right $ Response 200 "\"ab\""
      callEndpoint good textEndpoint `shouldReturn` Right "ab"
      large <- returning (defaultClientConfig { cc_maxResponseBytes = 3 }) $ Right $ Response 200 "\"ab\""
      callEndpoint large textEndpoint `shouldReturn` Left ResponseTooLarge
    it "curries path, parameter and body arguments and propagates the typed result" $ do
      captured <- newIORef Nothing
      client <- require $ newClient defaultClientConfig $ \request -> do
        writeIORef captured $ Just (rq_url request, rq_headers request)
        pure $ Right $ Response 200 "\"result\""
      callDocumentedEndpoint client documented "item" "q" (Just 3) ["tag"] "by" Nothing `shouldReturn` Right "result"
      readIORef captured `shouldReturn` Just ("/api/item?search=q&offset=3&tag=tag", [("X-Requested-By", "by")])
    it "does not swallow Haskell cancellation from a custom transport" $ do
      client <- require $ newClient defaultClientConfig (\_ -> throwIO ThreadKilled)
      callEndpoint client textEndpoint `shouldThrow` (== ThreadKilled)

route :: Path '[T.Text] 'Open
route = "api" <//> var

textEndpoint :: Endpoint '[] 'Nothing T.Text
textEndpoint = MethodGet "value"

documented :: DocumentedEndpoint '[T.Text] '[T.Text, Maybe Int, [T.Text], T.Text, Maybe T.Text] 'Nothing T.Text
documented = DocumentedEndpoint (MethodGet route) (operationInfo "get")
  (PathParameter (parameterInfo "id" textSchema) NoPathParameters)
  (QueryParam (parameterInfo "search" textSchema) :> OptionalQueryParam (parameterInfo "offset" intSchema) :>
   QueryList (parameterInfo "tag" textSchema) :> HeaderParam (parameterInfo "X-Requested-By" textSchema) :>
   OptionalHeaderParam (parameterInfo "X-Optional" textSchema) :> NoParameters)
  NoBody textSchema

data Custom = Custom
instance FromHttpApiData Custom where parseUrlPiece _ = Right Custom
instance ToHttpApiData Custom where
  toUrlPiece _ = "url-value"
  toQueryParam _ = "query-value"
  toHeader _ = "header-value"

custom :: DocumentedEndpoint '[] '[Custom, Custom] 'Nothing T.Text
custom = DocumentedEndpoint (MethodGet "custom") (operationInfo "custom") NoPathParameters
  (QueryParam (parameterInfo "query" $ schemaObject []) :> HeaderParam (parameterInfo "X-Custom" $ schemaObject []) :> NoParameters)
  NoBody textSchema

require :: Show e => Either e a -> IO a
require = either (fail . show) pure

errorOf :: Either e a -> Maybe e
errorOf = either Just (const Nothing)

unused :: Transport
unused _ = fail "unexpected transport"

returning :: ClientConfig -> Either ClientError Response -> IO Client
returning cfg result = require $ newClient cfg (const $ pure result)
