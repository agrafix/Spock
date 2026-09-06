{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ApiDefinitions (Item (..), getItem, patchItem, deleteItem, apiDocument) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Text as T
import Web.Spock.Api
import Web.Spock.Api.Document

data Item = Item { identifier :: Int, value :: Int, client :: Maybe T.Text }
  deriving (Generic, Show)
instance ToJSON Item
instance FromJSON Item

itemSchema :: Schema Item
itemSchema = schemaObject
  [ ("type", String "object"),
    ("properties", object ["identifier" .= schemaValue intSchema, "value" .= schemaValue intSchema,
                           "client" .= schemaValue (nullableSchema textSchema)]),
    ("required", toJSON (["identifier", "value", "client"] :: [T.Text])) ]

getItem :: DocumentedEndpoint '[Int] '[Maybe Int, Maybe T.Text] 'Nothing Item
getItem = DocumentedEndpoint (MethodGet $ "items" <//> var)
  ((operationInfo "getItem") { oi_summary = "Read an example item", oi_tags = ["items"] })
  (PathParameter (parameterInfo "id" intSchema) NoPathParameters)
  (OptionalQueryParam (parameterInfo "offset" intSchema) :> OptionalHeaderParam (parameterInfo "X-Client" textSchema) :> NoParameters)
  NoBody itemSchema

patchItem :: DocumentedEndpoint '[Int] '[] ('Just Int) Item
patchItem = DocumentedEndpoint (MethodPatch Proxy $ "items" <//> var)
  ((operationInfo "patchItem") { oi_summary = "Return an item with a new value", oi_tags = ["items"] })
  (PathParameter (parameterInfo "id" intSchema) NoPathParameters) NoParameters (JsonBody intSchema) itemSchema

deleteItem :: DocumentedEndpoint '[Int] '[] 'Nothing Bool
deleteItem = DocumentedEndpoint (MethodDelete $ "items" <//> var)
  ((operationInfo "deleteItem") { oi_summary = "Demonstrate a typed DELETE response", oi_tags = ["items"] })
  (PathParameter (parameterInfo "id" intSchema) NoPathParameters) NoParameters NoBody boolSchema

apiDocument :: Either OpenApiError Value
apiDocument = openApiDocument "Spock typed API example" "1.0.0"
  [SomeEndpoint getItem, SomeEndpoint patchItem, SomeEndpoint deleteItem]
