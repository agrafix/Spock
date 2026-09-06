{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Typed clients for the same endpoint definitions used by Spock's server.
-- Use @browserClient@ from @Web.Spock.Api.Client.Browser@ with GHC's JavaScript
-- backend, or supply a transport to 'newClient'. Errors are explicit values;
-- this module never prints response bodies or credentials.
module Web.Spock.Api.Client
  ( Client, ClientConfig (..), defaultClientConfig, Credentials (..),
    ClientError (..), Header, Request (..), Response (..), Transport, newClient,
    callEndpoint, callEndpoint', callDocumentedEndpoint, callDocumentedEndpoint',
    prepareEndpoint, prepareDocumentedEndpoint
  ) where

import Control.Monad (unless, when)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.HVect (HVect (..), HVectElim, HasRep, AllHave)
import qualified Data.HVect as HV
import Data.List (nub)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types.URI (renderQuery)
import Network.URI (URI (..), URIAuth (..), parseURIReference, unEscapeString)
import Web.HttpApiData (ToHttpApiData, toHeader, toQueryParam)
import Web.Spock.Api
import Web.Spock.Api.Document

-- | Extra headers are UTF-8 encoded. Typed HeaderParam values use their
-- ToHttpApiData.toHeader encoding. Duplicate names, including case variants,
-- and CR/LF or control bytes are rejected before invoking the transport.
type Header = (T.Text, T.Text)

data Credentials = SameOrigin | OmitCredentials | IncludeCredentials
  deriving (Eq, Show)

data ClientConfig = ClientConfig
  { cc_baseUrl :: T.Text,
    cc_headers :: [Header],
    cc_credentials :: Credentials,
    cc_timeoutMilliseconds :: Int,
    cc_maxResponseBytes :: Int,
    cc_slashPolicy :: SlashPolicy
  } deriving (Eq)

-- | Same-origin URLs and cookies, a 30-second timeout and 1 MiB response limit.
-- The browser transport enforces time and streaming byte limits. Custom
-- transports must implement the timeout; the decoder also checks body size.
defaultClientConfig :: ClientConfig
defaultClientConfig = ClientConfig "" [] SameOrigin 30000 (1024 * 1024) IgnoreSlashes

-- | Errors omit request headers, URLs, payloads and server response bodies.
data ClientError = InvalidClientConfig | InvalidEndpoint | InvalidRequest
  | NetworkFailure | RequestTimedOut | ResponseTooLarge | HttpError Int | DecodeFailure
  deriving (Eq, Show)

-- | A prepared request. Treat headers/body as sensitive; it has no Show instance.
data Request = Request
  { rq_method :: T.Text,
    rq_url :: T.Text,
    rq_headers :: [(B.ByteString, B.ByteString)],
    rq_body :: Maybe B.ByteString,
    rq_credentials :: Credentials,
    rq_timeoutMilliseconds :: Int,
    rq_maxResponseBytes :: Int
  }

data Response = Response { rs_status :: Int, rs_body :: B.ByteString }

type Transport = Request -> IO (Either ClientError Response)

data Client = Client ClientConfig Transport

-- | Validate configuration before any request. Base URLs may be an empty
-- same-origin prefix, an absolute path prefix, or an http(s) URL. Query strings,
-- fragments, embedded credentials, protocol-relative URLs and backslashes are
-- rejected. Cross-origin browser calls still require the server's CORS policy.
newClient :: ClientConfig -> Transport -> Either ClientError Client
newClient cfg transport = do
  unless (validBaseUrl $ cc_baseUrl cfg) $ Left InvalidClientConfig
  unless (cc_timeoutMilliseconds cfg > 0 && cc_timeoutMilliseconds cfg <= 2147483647
    && cc_maxResponseBytes cfg > 0) $ Left InvalidClientConfig
  either (const $ Left InvalidClientConfig) pure $ validateHeaders $ encodeHeaders $ cc_headers cfg
  pure (Client cfg transport)

-- | Arguments follow path capture order, then the JSON body (if present).
-- Any 2xx response must contain JSON matching the endpoint's result type.
-- Non-2xx responses yield HttpError; malformed or empty JSON yields DecodeFailure.
callEndpoint :: (HasRep p, HasRep (MaybeToList i), AllHave ToHttpApiData p) =>
  Client -> Endpoint p i o -> HVectElim p (HVectElim (MaybeToList i) (IO (Either ClientError o)))
callEndpoint client endpoint = callEndpoint' client endpoint []

-- | As 'callEndpoint', with extra headers such as an explicit CSRF token.
callEndpoint' :: forall p i o. (HasRep p, HasRep (MaybeToList i), AllHave ToHttpApiData p) =>
  Client -> Endpoint p i o -> [Header] -> HVectElim p (HVectElim (MaybeToList i) (IO (Either ClientError o)))
callEndpoint' client@(Client cfg _) endpoint extra =
  HV.curry $ \path -> HV.curry $ \body ->
    perform client endpoint (prepareEndpoint cfg endpoint extra path body)

-- | Arguments are path captures, declared query/header parameters, then body.
-- Optional parameters are omitted for Nothing; repeated query values preserve
-- order. Encoders are carried by the shared Parameter definitions.
callDocumentedEndpoint :: (HasRep p, HasRep q, HasRep (MaybeToList i), AllHave ToHttpApiData p) =>
  Client -> DocumentedEndpoint p q i o -> HVectElim p (HVectElim q (HVectElim (MaybeToList i) (IO (Either ClientError o))))
callDocumentedEndpoint client endpoint = callDocumentedEndpoint' client endpoint []

callDocumentedEndpoint' :: forall p q i o. (HasRep p, HasRep q, HasRep (MaybeToList i), AllHave ToHttpApiData p) =>
  Client -> DocumentedEndpoint p q i o -> [Header] -> HVectElim p (HVectElim q (HVectElim (MaybeToList i) (IO (Either ClientError o))))
callDocumentedEndpoint' client@(Client cfg _) endpoint extra =
  HV.curry $ \path -> HV.curry $ \parameters -> HV.curry $ \body ->
    perform client (de_endpoint endpoint) (prepareDocumentedEndpoint cfg endpoint extra path parameters body)

-- | Prepare an encoded request without sending it; useful with custom transports.
prepareEndpoint :: forall p i o. AllHave ToHttpApiData p => ClientConfig -> Endpoint p i o -> [Header] ->
  HVect p -> HVect (MaybeToList i) -> Either ClientError Request
prepareEndpoint cfg endpoint extra path body = case endpoint of
  MethodGet route -> case body of HNil -> make "GET" route Nothing
  MethodDelete route -> case body of HNil -> make "DELETE" route Nothing
  MethodPost _ route -> case body of value :&: HNil -> make "POST" route (Just $ jsonBytes value)
  MethodPut _ route -> case body of value :&: HNil -> make "PUT" route (Just $ jsonBytes value)
  MethodPatch _ route -> case body of value :&: HNil -> make "PATCH" route (Just $ jsonBytes value)
  where
    make :: T.Text -> Path p 'Open -> Maybe B.ByteString -> Either ClientError Request
    make method route payload = do
      _ <- newClient cfg (const $ pure $ Left NetworkFailure)
      let headers = encodeHeaders (cc_headers cfg ++ extra) ++
            [("Content-Type", "application/json;charset=UTF-8") | maybe False (const True) payload]
          url = T.dropWhileEnd (== '/') (cc_baseUrl cfg) <> "/" <> renderRouteEncodedWith (cc_slashPolicy cfg) route path
      validateHeaders headers
      unless (validBaseUrl url) $ Left InvalidRequest
      pure $ Request method url headers payload (cc_credentials cfg) (cc_timeoutMilliseconds cfg) (cc_maxResponseBytes cfg)

prepareDocumentedEndpoint :: AllHave ToHttpApiData p => ClientConfig -> DocumentedEndpoint p q i o -> [Header] ->
  HVect p -> HVect q -> HVect (MaybeToList i) -> Either ClientError Request
prepareDocumentedEndpoint cfg endpoint extra path parameters body = do
  either (const $ Left InvalidEndpoint) pure $ validateEndpoint endpoint
  request <- prepareEndpoint cfg (de_endpoint endpoint) extra path body
  let (query, headers) = parameterValues (de_parameters endpoint) parameters
      combined = rq_headers request ++ headers
  validateHeaders combined
  pure request { rq_url = rq_url request <> T.decodeUtf8 (renderQuery True query), rq_headers = combined }

parameterValues :: Parameters q -> HVect q -> ([(B.ByteString, Maybe B.ByteString)], [(B.ByteString, B.ByteString)])
parameterValues NoParameters HNil = ([], [])
parameterValues (parameter :> rest) (value :&: values) =
  let (query, headers) = parameterValues rest values
  in case parameter of
    QueryParam info -> (queryValue (pi_name info) value : query, headers)
    OptionalQueryParam info -> (maybe [] (\v -> [queryValue (pi_name info) v]) value ++ query, headers)
    QueryList info -> (map (queryValue $ pi_name info) value ++ query, headers)
    HeaderParam info -> (query, headerValue (pi_name info) value : headers)
    OptionalHeaderParam info -> (query, maybe [] (\v -> [headerValue (pi_name info) v]) value ++ headers)

queryValue :: ToHttpApiData a => T.Text -> a -> (B.ByteString, Maybe B.ByteString)
queryValue name value = (T.encodeUtf8 name, Just $ T.encodeUtf8 $ toQueryParam value)

headerValue :: ToHttpApiData a => T.Text -> a -> (B.ByteString, B.ByteString)
headerValue name value = (T.encodeUtf8 name, toHeader value)

jsonBytes :: A.ToJSON a => a -> B.ByteString
jsonBytes = BL.toStrict . A.encode

perform :: Client -> Endpoint p i o -> Either ClientError Request -> IO (Either ClientError o)
perform _ _ (Left err) = pure (Left err)
perform (Client _ transport) endpoint (Right request) = do
  result <- transport request
  pure $ result >>= \response -> decodeResponse endpoint (rq_maxResponseBytes request) response

decodeResponse :: Endpoint p i o -> Int -> Response -> Either ClientError o
decodeResponse endpoint limit response = case endpoint of
  MethodGet _ -> decode
  MethodDelete _ -> decode
  MethodPost _ _ -> decode
  MethodPut _ _ -> decode
  MethodPatch _ _ -> decode
  where
    decode :: A.FromJSON a => Either ClientError a
    decode
      | rs_status response < 200 || rs_status response >= 300 = Left $ HttpError $ rs_status response
      | B.length (rs_body response) > limit = Left ResponseTooLarge
      | otherwise = either (const $ Left DecodeFailure) Right $ A.eitherDecodeStrict' $ rs_body response

encodeHeaders :: [Header] -> [(B.ByteString, B.ByteString)]
encodeHeaders = map (\(name, value) -> (T.encodeUtf8 name, T.encodeUtf8 value))

validateHeaders :: [(B.ByteString, B.ByteString)] -> Either ClientError ()
validateHeaders headers = do
  let names = map (B.map lower . fst) headers
      validName name = not (B.null name) && B.all (\c -> asciiAlphaNum c || c `B.elem` "!#$%&'*+-.^_`|~") name
      validValue = B.all (\c -> c == 9 || c >= 32 && c /= 127)
      lower c | c >= 65 && c <= 90 = c + 32
              | otherwise = c
      asciiAlphaNum c = c >= 65 && c <= 90 || c >= 97 && c <= 122 || c >= 48 && c <= 57
  when (length names /= length (nub names) || any (\(n,v) -> not (validName n && validValue v)) headers) $ Left InvalidRequest

validBaseUrl :: T.Text -> Bool
validBaseUrl value
  | T.any (\c -> c <= ' ' || c == '\\' || c == '\DEL') value = False
  | otherwise = case parseURIReference (T.unpack value) of
      Just uri | null (uriQuery uri) && null (uriFragment uri),
        not (any (\piece -> unEscapeString (T.unpack piece) `elem` [".", ".."]) $ T.splitOn "/" $ T.pack $ uriPath uri) -> case (uriScheme uri, uriAuthority uri) of
        ("", Nothing) -> T.null value || T.isPrefixOf "/" value
        (scheme, Just authority) -> scheme `elem` ["http:", "https:"] && null (uriUserInfo authority) && not (null $ uriRegName authority)
        _ -> False
      _ -> False
