{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Typed parameter and schema metadata shared by server registration and
-- OpenAPI generation. Schemas describe JSON encodings; custom schemas should
-- match the application's ToJSON/FromJSON instances.
module Web.Spock.Api.Document
  ( Schema, schemaObject, schemaValue, textSchema, intSchema, integerSchema,
    boolSchema, doubleSchema, arraySchema, nullableSchema,
    ParameterInfo (..), parameterInfo, PathParameters (..), Parameter (..), Parameters (..),
    BodySchema (..), OperationInfo (..), operationInfo,
    DocumentedEndpoint (..), SomeEndpoint (..), OpenApiError (..),
    validateEndpoint, openApiDocument,
  ) where

import Control.Monad (foldM, forM_, unless, when)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types.URI (urlEncode)
import Web.HttpApiData (FromHttpApiData)
import Web.Routing.Combinators (PathState (Open))
import Web.Spock.Api

newtype Schema a = Schema { schemaValue :: Value }

-- | Construct a custom JSON Schema object (OpenAPI 3.1 / JSON Schema 2020-12).
-- The caller is responsible for its validity and correspondence to the codec.
schemaObject :: [(Text, Value)] -> Schema a
schemaObject fields = Schema $ Object $ KM.fromList [(Key.fromText key, value) | (key, value) <- fields]

textSchema :: Schema Text
textSchema = schemaObject [("type", String "string")]
intSchema :: Schema Int
intSchema = schemaObject [("type", String "integer")]
integerSchema :: Schema Integer
integerSchema = schemaObject [("type", String "integer")]
boolSchema :: Schema Bool
boolSchema = schemaObject [("type", String "boolean")]
doubleSchema :: Schema Double
doubleSchema = schemaObject [("type", String "number"), ("format", String "double")]
arraySchema :: Schema a -> Schema [a]
arraySchema item = schemaObject [("type", String "array"), ("items", schemaValue item)]
nullableSchema :: Schema a -> Schema (Maybe a)
nullableSchema value = Schema $ object ["anyOf" .= [schemaValue value, object ["type" .= ("null" :: Text)]]]

data ParameterInfo a = ParameterInfo
  { pi_name :: Text, pi_description :: Text, pi_schema :: Schema a }

parameterInfo :: Text -> Schema a -> ParameterInfo a
parameterInfo name = ParameterInfo name ""

-- | Exactly one name and schema for each captured path value, in path order.
data PathParameters (p :: [Type]) where
  NoPathParameters :: PathParameters '[]
  PathParameter :: ParameterInfo a -> PathParameters p -> PathParameters (a ': p)

data Parameter a where
  QueryParam :: FromHttpApiData a => ParameterInfo a -> Parameter a
  OptionalQueryParam :: FromHttpApiData a => ParameterInfo a -> Parameter (Maybe a)
  -- | Repeated @?name=one&name=two@ values, preserving order. Missing means [].
  QueryList :: FromHttpApiData a => ParameterInfo a -> Parameter [a]
  HeaderParam :: FromHttpApiData a => ParameterInfo a -> Parameter a
  OptionalHeaderParam :: FromHttpApiData a => ParameterInfo a -> Parameter (Maybe a)

data Parameters (q :: [Type]) where
  NoParameters :: Parameters '[]
  (:>) :: Parameter a -> Parameters q -> Parameters (a ': q)
infixr 5 :>

data BodySchema (i :: Maybe Type) where
  NoBody :: BodySchema 'Nothing
  JsonBody :: Schema a -> BodySchema ('Just a)

data OperationInfo = OperationInfo
  { oi_operationId :: Text, oi_summary :: Text, oi_description :: Text,
    oi_tags :: [Text], oi_deprecated :: Bool }

operationInfo :: Text -> OperationInfo
operationInfo name = OperationInfo name "" "" [] False

data DocumentedEndpoint p q i o = DocumentedEndpoint
  { de_endpoint :: Endpoint p i o,
    de_operation :: OperationInfo,
    de_pathParameters :: PathParameters p,
    de_parameters :: Parameters q,
    de_body :: BodySchema i,
    de_response :: Schema o }

data SomeEndpoint where
  SomeEndpoint :: DocumentedEndpoint p q i o -> SomeEndpoint

newtype OpenApiError = OpenApiError Text deriving (Eq, Show)

validateEndpoint :: DocumentedEndpoint p q i o -> Either OpenApiError ()
validateEndpoint endpoint = do
  unless (not $ T.null $ T.strip $ oi_operationId $ de_operation endpoint) $
    Left $ OpenApiError "Operation ID must not be empty"
  let pathNames = pathParameterNames $ de_pathParameters endpoint
      parameters = parameterNames $ de_parameters endpoint
  forM_ pathNames $ \name -> unless (validPathName name) $
    Left $ OpenApiError "Path parameter names must contain only ASCII letters, digits, underscores, dots, or hyphens"
  unless (unique pathNames) $ Left $ OpenApiError "Duplicate path parameter name"
  forM_ parameters $ \(location, name) -> do
    when (T.null name) $ Left $ OpenApiError "Parameter name must not be empty"
    when (location == "header" && not (validHeaderName name)) $
      Left $ OpenApiError "Invalid header parameter name"
    when (location == "header" && T.toCaseFold name `elem` ["accept", "content-type", "authorization"]) $
      Left $ OpenApiError "OpenAPI reserves Accept, Content-Type, and Authorization headers; use content or security metadata instead"
  unless (unique [(location, if location == "header" then T.toCaseFold name else name) | (location, name) <- parameters]) $
    Left $ OpenApiError "Duplicate query or header parameter"
  where
    unique :: Ord a => [a] -> Bool
    unique values = Set.size (Set.fromList values) == length values

-- | Generate OpenAPI 3.1.1 with JSON request/response schemas and typed path,
-- query, and header parameters. Conflicting operation IDs, duplicate methods,
-- and equivalent templates using different path names are rejected.
openApiDocument :: Text -> Text -> [SomeEndpoint] -> Either OpenApiError Value
openApiDocument title version endpoints = do
  (paths, _, _) <- foldM add (Map.empty, Set.empty, Map.empty) endpoints
  pure $ object ["openapi" .= ("3.1.1" :: Text), "info" .= object ["title" .= title, "version" .= version],
    "paths" .= Object (KM.fromList [(Key.fromText path, Object $ KM.fromList [(Key.fromText method, value) | (method, value) <- Map.toList methods])
                                  | (path, methods) <- Map.toList paths])]
  where
    add (paths, operations, templates) (SomeEndpoint endpoint) = do
      validateEndpoint endpoint
      let info = de_operation endpoint
          opId = oi_operationId info
          (method, path) = endpointRoute $ de_endpoint endpoint
          (rendered, template, pathParams) = describePath path (de_pathParameters endpoint)
          methods = Map.findWithDefault Map.empty rendered paths
      when (Set.member opId operations) $ Left $ OpenApiError ("Duplicate operation ID: " <> opId)
      when (Map.member method methods) $ Left $ OpenApiError ("Duplicate endpoint: " <> method <> " " <> rendered)
      case Map.lookup template templates of
        Just old | old /= rendered -> Left $ OpenApiError ("Conflicting path templates: " <> old <> " and " <> rendered)
        _ -> pure ()
      let operation = object $
            [ "operationId" .= opId, "summary" .= oi_summary info, "description" .= oi_description info,
              "tags" .= oi_tags info, "deprecated" .= oi_deprecated info,
              "parameters" .= (pathParams ++ describeParameters (de_parameters endpoint)),
              "responses" .= object
                [ "200" .= object ["description" .= ("Successful response" :: Text),
                    "content" .= jsonContent (de_response endpoint)],
                  "400" .= object ["description" .= ("Invalid query/header parameters or JSON body" :: Text)] ] ]
            ++ describeBody (de_body endpoint)
      pure (Map.insert rendered (Map.insert method operation methods) paths,
            Set.insert opId operations, Map.insert template rendered templates)

endpointRoute :: Endpoint p i o -> (Text, Path p 'Open)
endpointRoute (MethodGet path) = ("get", path)
endpointRoute (MethodPost _ path) = ("post", path)
endpointRoute (MethodPut _ path) = ("put", path)
endpointRoute (MethodPatch _ path) = ("patch", path)
endpointRoute (MethodDelete path) = ("delete", path)

describePath :: Path p 'Open -> PathParameters p -> (Text, Text, [Value])
describePath path parameters = let (pieces, template, values) = go path parameters
  in ("/" <> T.intercalate "/" pieces, "/" <> T.intercalate "/" template, values)
  where
    go :: Path as 'Open -> PathParameters as -> ([Text], [Text], [Value])
    go Empty NoPathParameters = ([], [], [])
    go (StaticCons piece rest) params =
      let (pieces, template, values) = go rest params
          encoded = T.decodeUtf8 $ urlEncode True $ T.encodeUtf8 piece
      in (encoded : pieces, encoded : template, values)
    go (VarCons rest) (PathParameter info params) =
      let (pieces, template, values) = go rest params
      in (("{" <> pi_name info <> "}") : pieces, "{}" : template,
          describeParameter "path" True "simple" False info : values)

describeParameters :: Parameters q -> [Value]
describeParameters NoParameters = []
describeParameters (parameter :> rest) = describe parameter : describeParameters rest
  where
    describe :: Parameter a -> Value
    describe (QueryParam info) = describeParameter "query" True "form" True info
    describe (OptionalQueryParam info) = describeParameter "query" False "form" True info
    describe (QueryList info) = describeParameter "query" False "form" True
      (ParameterInfo (pi_name info) (pi_description info) $ arraySchema $ pi_schema info)
    describe (HeaderParam info) = describeParameter "header" True "simple" False info
    describe (OptionalHeaderParam info) = describeParameter "header" False "simple" False info

describeParameter :: Text -> Bool -> Text -> Bool -> ParameterInfo a -> Value
describeParameter location required style explode info = object
  ["name" .= pi_name info, "in" .= location, "required" .= required,
   "description" .= pi_description info, "schema" .= schemaValue (pi_schema info),
   "style" .= style, "explode" .= explode]

describeBody :: BodySchema i -> [(Key.Key, Value)]
describeBody NoBody = []
describeBody (JsonBody schema) = ["requestBody" .= object ["required" .= True, "content" .= jsonContent schema]]

jsonContent :: Schema a -> Value
jsonContent schema = object ["application/json" .= object ["schema" .= schemaValue schema]]

pathParameterNames :: PathParameters p -> [Text]
pathParameterNames NoPathParameters = []
pathParameterNames (PathParameter info rest) = pi_name info : pathParameterNames rest

parameterNames :: Parameters q -> [(Text, Text)]
parameterNames NoParameters = []
parameterNames (parameter :> rest) = name parameter : parameterNames rest
  where
    name :: Parameter a -> (Text, Text)
    name (QueryParam info) = ("query", pi_name info)
    name (OptionalQueryParam info) = ("query", pi_name info)
    name (QueryList info) = ("query", pi_name info)
    name (HeaderParam info) = ("header", pi_name info)
    name (OptionalHeaderParam info) = ("header", pi_name info)

validPathName :: Text -> Bool
validPathName name = not (T.null name) && BS.all
  (\c -> asciiAlphaNum c || c `elem` [45, 46, 95]) (T.encodeUtf8 name)

validHeaderName :: Text -> Bool
validHeaderName name = not (T.null name) && BS.all
  (\c -> asciiAlphaNum c || c `BS.elem` "!#$%&'*+-.^_`|~") (T.encodeUtf8 name)

asciiAlphaNum :: (Ord a, Num a) => a -> Bool
asciiAlphaNum c = (c >= 65 && c <= 90) || (c >= 97 && c <= 122) || (c >= 48 && c <= 57)
