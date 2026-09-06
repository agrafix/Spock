{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Either (isLeft)
import qualified Data.Text as T
import qualified Data.Vector as Vector
import Test.Hspec
import Web.Spock.Api
import Web.Spock.Api.Document

main :: IO ()
main = hspec $ describe "OpenAPI generation" $ do
  it "merges methods on the same path and emits body and response schemas" $ do
    doc <- requireDocument [SomeEndpoint getItem, SomeEndpoint patchItem, SomeEndpoint deleteItem]
    at ["openapi"] doc `shouldBe` String "3.1.1"
    at ["paths", "/items/{id}", "get", "operationId"] doc `shouldBe` String "getItem"
    at ["paths", "/items/{id}", "patch", "requestBody", "required"] doc `shouldBe` Bool True
    at ["paths", "/items/{id}", "patch", "requestBody", "content", "application/json", "schema"] doc `shouldBe` schemaValue intSchema
    at ["paths", "/items/{id}", "delete", "responses", "200", "content", "application/json", "schema"] doc `shouldBe` schemaValue boolSchema
    at ["paths", "/items/{id}", "get", "requestBody"] doc `shouldBe` Null

  it "describes path, optional query, and required header parameters in order" $ do
    doc <- requireDocument [SomeEndpoint getItem]
    case at ["paths", "/items/{id}", "get", "parameters"] doc of
      Array values -> do
        map (at ["name"]) (Vector.toList values) `shouldBe` map String ["id", "offset", "X-Client"]
        map (at ["in"]) (Vector.toList values) `shouldBe` map String ["path", "query", "header"]
        map (at ["required"]) (Vector.toList values) `shouldBe` map Bool [True, False, True]
      value -> expectationFailure $ show value

  it "describes repeated query values as exploded arrays" $ do
    let endpoint = DocumentedEndpoint (MethodGet "items" :: Endpoint '[] 'Nothing [Int])
          (operationInfo "listItems") NoPathParameters
          (QueryList (parameterInfo "id" intSchema) :> NoParameters) NoBody (arraySchema intSchema)
    doc <- requireDocument [SomeEndpoint endpoint]
    case at ["paths", "/items", "get", "parameters"] doc of
      Array values -> do
        let parameter = values Vector.! 0
        at ["schema", "type"] parameter `shouldBe` String "array"
        at ["schema", "items"] parameter `shouldBe` schemaValue intSchema
        at ["explode"] parameter `shouldBe` Bool True
        at ["required"] parameter `shouldBe` Bool False
      value -> expectationFailure $ show value

  it "retains operation descriptions, tags, and deprecation metadata" $ do
    let endpoint = getItem { de_operation = (operationInfo "oldItem")
          { oi_summary = "Read item", oi_description = "Detailed description", oi_tags = ["items"], oi_deprecated = True } }
    doc <- requireDocument [SomeEndpoint endpoint]
    let operation = at ["paths", "/items/{id}", "get"] doc
    at ["summary"] operation `shouldBe` String "Read item"
    at ["description"] operation `shouldBe` String "Detailed description"
    at ["tags"] operation `shouldBe` toJSON (["items"] :: [T.Text])
    at ["deprecated"] operation `shouldBe` Bool True

  it "renders root and percent-encodes literal path segments" $ do
    let rootEndpoint = DocumentedEndpoint (MethodGet Empty :: Endpoint '[] 'Nothing T.Text)
          (operationInfo "root") NoPathParameters NoParameters NoBody textSchema
        escaped = rootEndpoint { de_endpoint = MethodGet (StaticCons "a b/{literal}" Empty), de_operation = operationInfo "escaped" }
    doc <- requireDocument [SomeEndpoint rootEndpoint, SomeEndpoint escaped]
    at ["paths", "/", "get", "operationId"] doc `shouldBe` String "root"
    at ["paths", "/a%20b%2F%7Bliteral%7D", "get", "operationId"] doc `shouldBe` String "escaped"

  it "rejects duplicate operation IDs" $
    openApiDocument "API" "1" [SomeEndpoint getItem, SomeEndpoint (patchItem { de_operation = de_operation getItem })] `shouldSatisfy` isLeft
  it "rejects duplicate path/method registrations" $
    openApiDocument "API" "1" [SomeEndpoint getItem, SomeEndpoint (getItem { de_operation = operationInfo "another" })] `shouldSatisfy` isLeft
  it "rejects equivalent templates with inconsistent capture names" $
    openApiDocument "API" "1" [SomeEndpoint getItem, SomeEndpoint (patchItem { de_pathParameters = PathParameter (parameterInfo "otherId" intSchema) NoPathParameters })] `shouldSatisfy` isLeft
  it "rejects empty operation IDs and invalid capture names" $ do
    validateEndpoint (getItem { de_operation = operationInfo "" }) `shouldSatisfy` isLeft
    validateEndpoint (getItem { de_pathParameters = PathParameter (parameterInfo "bad/name" intSchema) NoPathParameters }) `shouldSatisfy` isLeft
  it "rejects duplicate header names case-insensitively" $ do
    let endpoint = getItem { de_parameters = HeaderParam (parameterInfo "X-Client" intSchema)
          :> OptionalHeaderParam (parameterInfo "x-client" textSchema) :> NoParameters }
    validateEndpoint endpoint `shouldSatisfy` isLeft
  it "rejects duplicate query names" $ do
    let endpoint = getItem { de_parameters = QueryParam (parameterInfo "limit" intSchema)
          :> QueryList (parameterInfo "limit" intSchema) :> NoParameters }
    validateEndpoint endpoint `shouldSatisfy` isLeft
  it "rejects invalid or OpenAPI-reserved header parameter names" $ do
    let endpoint name = getItem { de_parameters = HeaderParam (parameterInfo name textSchema) :> NoParameters }
    mapM_ (\name -> validateEndpoint (endpoint name) `shouldSatisfy` isLeft) ["", "bad header", "Authorization", "Content-Type", "Accept"]
  it "supports nullable and custom JSON schemas" $ do
    schemaValue (nullableSchema textSchema) `shouldBe` object ["anyOf" .= [object ["type" .= ("string" :: T.Text)], object ["type" .= ("null" :: T.Text)]]]
    schemaValue (schemaObject [("type", String "object"), ("additionalProperties", Bool False)] :: Schema Value)
      `shouldBe` object ["type" .= ("object" :: T.Text), "additionalProperties" .= False]

getItem :: DocumentedEndpoint '[Int] '[Maybe Int, T.Text] 'Nothing T.Text
getItem = DocumentedEndpoint (MethodGet $ "items" <//> var) (operationInfo "getItem")
  (PathParameter (parameterInfo "id" intSchema) NoPathParameters)
  (OptionalQueryParam (parameterInfo "offset" intSchema) :> HeaderParam (parameterInfo "X-Client" textSchema) :> NoParameters)
  NoBody textSchema

patchItem :: DocumentedEndpoint '[Int] '[] ('Just Int) Int
patchItem = DocumentedEndpoint (MethodPatch Proxy $ "items" <//> var) (operationInfo "patchItem")
  (PathParameter (parameterInfo "id" intSchema) NoPathParameters) NoParameters (JsonBody intSchema) intSchema

deleteItem :: DocumentedEndpoint '[Int] '[] 'Nothing Bool
deleteItem = DocumentedEndpoint (MethodDelete $ "items" <//> var) (operationInfo "deleteItem")
  (PathParameter (parameterInfo "id" intSchema) NoPathParameters) NoParameters NoBody boolSchema

at :: [T.Text] -> Value -> Value
at [] value = value
at (key : rest) (Object value) = maybe Null (at rest) (KM.lookup (Key.fromText key) value)
at _ _ = Null

requireDocument :: [SomeEndpoint] -> IO Value
requireDocument = either (fail . show) pure . openApiDocument "Test API" "1.0"
