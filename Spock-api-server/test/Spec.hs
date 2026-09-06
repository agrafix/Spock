{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson (Value (..), eitherDecode, object, toJSON, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai
import Test.Hspec
import Web.Spock.Api (Endpoint (..), Proxy (..))
import Web.Spock.Api.Document
import Web.Spock.Api.Server
import Web.Spock.Core hiding (request)

main :: IO ()
main = hspec $ before apiApp $ describe "Typed API requests" $ do
  it "keeps GET, POST, and PUT endpoint behavior" $ \app -> do
    expectJSON app "GET" "/legacy/3" [] "" (Number 3)
    expectJSON app "POST" "/legacy/3" [] "4" (Number 7)
    expectJSON app "PUT" "/legacy/3" [] "5" (Number 8)
  it "routes DELETE with typed path arguments" $ \app ->
    expectJSON app "DELETE" "/legacy/3" [] "" (Bool True)
  it "routes PATCH with typed path arguments and a JSON body" $ \app ->
    expectJSON app "PATCH" "/legacy/3" [] "9" (Number 12)
  it "rejects invalid JSON without running the handler" $ \app -> do
    result <- send app "PATCH" "/legacy/3" [] "not-json"
    Wai.simpleStatus result `shouldBe` status400
  it "passes typed optional query and required header values after path captures" $ \app ->
    expectJSON app "GET" "/items/3?offset=4" [("X-Client", "test")] "" (String "7:test")
  it "uses Nothing for an absent optional query parameter" $ \app ->
    expectJSON app "GET" "/items/3" [("x-client", "test")] "" (String "3:test")
  it "matches headers case-insensitively" $ \app ->
    expectJSON app "GET" "/items/3" [("x-ClIeNt", "agent")] "" (String "3:agent")
  it "reports missing required headers" $ \app -> expectBad app "/items/3" [] "Missing header parameter: X-Client"
  it "rejects duplicate scalar query values" $ \app -> expectBad app "/items/3?offset=1&offset=2" [("X-Client", "test")] "Duplicate query parameter: offset"
  it "rejects duplicate scalar headers, including case variations" $ \app -> expectBad app "/items/3" [("X-Client", "one"), ("x-client", "two")] "Duplicate header parameter: X-Client"
  it "rejects malformed numbers without echoing the supplied value" $ \app -> expectBad app "/items/3?offset=private-value" [("X-Client", "test")] "Invalid query parameter: offset"
  it "rejects invalid UTF-8 query values" $ \app -> expectBad app "/items/3?offset=%FF" [("X-Client", "test")] "Invalid query parameter: offset"
  it "preserves repeated query order and uses [] when missing" $ \app -> do
    expectJSON app "GET" "/list?id=3&id=1&id=2" [] "" (toJSONInts [3, 1, 2])
    expectJSON app "GET" "/list" [] "" (toJSONInts [])
  it "rejects invalid members of repeated query parameters" $ \app -> expectBad app "/list?id=1&id=nope" [] "Invalid query parameter: id"
  it "handles required query and optional typed header parameters" $ \app -> do
    expectJSON app "GET" "/search?q=hello%20world" [] "" (String "hello world:0")
    expectJSON app "GET" "/search?q=hello" [("X-Limit", "4")] "" (String "hello:4")
    expectBad app "/search" [] "Missing query parameter: q"
    expectBad app "/search?q=hello" [("X-Limit", "bad")] "Invalid header parameter: X-Limit"
  it "distinguishes an explicit empty query value from a missing parameter" $ \app -> do
    expectJSON app "GET" "/search?q=" [] "" (String ":0")
    expectJSON app "GET" "/search?q" [] "" (String ":0")
  it "rejects invalid UTF-8 text headers" $ \app ->
    expectBad app "/items/3" [("X-Client", "\xff")] "Invalid header parameter: X-Client"
  it "wires documented methods and body arguments" $ \app -> do
    expectJSON app "PATCH" "/items/3?offset=4" [("X-Client", "test")] "5" (String "12:test")
    expectJSON app "DELETE" "/items/3" [("X-Client", "test")] "" (String "3:test")
  it "keeps invalid path captures from reaching handlers" $ \app -> do
    result <- send app "GET" "/items/not-an-int" [("X-Client", "test")] ""
    Wai.simpleStatus result `shouldBe` status404
  it "rejects invalid metadata during registration" $ \_ ->
    spockAsApp (spockT id $ defDocumentedEndpoint (item { de_operation = operationInfo "" })
      (\a offset client -> pure $ renderItem a offset client)) `shouldThrow` anyIOException

item :: DocumentedEndpoint '[Int] '[Maybe Int, T.Text] 'Nothing T.Text
item = DocumentedEndpoint (MethodGet $ "items" <//> var) (operationInfo "item")
  (PathParameter (parameterInfo "id" intSchema) NoPathParameters)
  (OptionalQueryParam (parameterInfo "offset" intSchema) :> HeaderParam (parameterInfo "X-Client" textSchema) :> NoParameters)
  NoBody textSchema

apiApp :: IO Wai.Application
apiApp = spockAsApp $ spockT id $ do
  defEndpoint (MethodGet ("legacy" <//> var) :: Endpoint '[Int] 'Nothing Int) pure
  defEndpoint (MethodPost Proxy ("legacy" <//> var) :: Endpoint '[Int] ('Just Int) Int) $ \a b -> pure (a + b)
  defEndpoint (MethodPut Proxy ("legacy" <//> var) :: Endpoint '[Int] ('Just Int) Int) $ \a b -> pure (a + b)
  defEndpoint (MethodPatch Proxy ("legacy" <//> var) :: Endpoint '[Int] ('Just Int) Int) $ \a b -> pure (a + b)
  defEndpoint (MethodDelete ("legacy" <//> var) :: Endpoint '[Int] 'Nothing Bool) $ \a -> pure (a > 0)
  defDocumentedEndpoint item $ \a offset client -> pure $ renderItem a offset client
  defDocumentedEndpoint (item { de_endpoint = MethodDelete ("items" <//> var), de_operation = operationInfo "deleteItem" }) $
    \a offset client -> pure $ renderItem a offset client
  defDocumentedEndpoint (item { de_endpoint = MethodPatch Proxy ("items" <//> var), de_body = JsonBody intSchema, de_operation = operationInfo "patchItem" }) $
    \a offset client bodyValue -> pure $ renderItem (a + bodyValue) offset client
  let list = DocumentedEndpoint (MethodGet "list" :: Endpoint '[] 'Nothing [Int]) (operationInfo "list") NoPathParameters
        (QueryList (parameterInfo "id" intSchema) :> NoParameters) NoBody (arraySchema intSchema)
      search = DocumentedEndpoint (MethodGet "search" :: Endpoint '[] 'Nothing T.Text) (operationInfo "search") NoPathParameters
        (QueryParam (parameterInfo "q" textSchema) :> OptionalHeaderParam (parameterInfo "X-Limit" intSchema) :> NoParameters) NoBody textSchema
  defDocumentedEndpoint list pure
  defDocumentedEndpoint search $ \q limit -> pure $ q <> ":" <> T.pack (show $ fromMaybe 0 limit)

renderItem :: Int -> Maybe Int -> T.Text -> T.Text
renderItem a offset client = T.pack (show $ a + fromMaybe 0 offset) <> ":" <> client

send :: Wai.Application -> BS.ByteString -> BS.ByteString -> RequestHeaders -> LBS.ByteString -> IO Wai.SResponse
send app method path headers payload = Wai.runSession (Wai.srequest $ Wai.SRequest
  ((Wai.setPath Wai.defaultRequest path) { Wai.requestMethod = method, Wai.requestHeaders = ("Content-Type", "application/json") : headers }) payload) app

expectJSON :: Wai.Application -> BS.ByteString -> BS.ByteString -> RequestHeaders -> LBS.ByteString -> Value -> Expectation
expectJSON app method path headers payload expected = do
  result <- send app method path headers payload
  Wai.simpleStatus result `shouldBe` status200
  eitherDecode (Wai.simpleBody result) `shouldBe` Right expected

expectBad :: Wai.Application -> BS.ByteString -> RequestHeaders -> T.Text -> Expectation
expectBad app path headers expected = do
  result <- send app "GET" path headers ""
  Wai.simpleStatus result `shouldBe` status400
  eitherDecode (Wai.simpleBody result) `shouldBe` Right (object ["error" .= expected])

toJSONInts :: [Int] -> Value
toJSONInts = toJSON
