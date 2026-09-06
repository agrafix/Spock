{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Spock.Api.Server (defEndpoint, defDocumentedEndpoint) where

import Control.Monad.Trans
import Data.Aeson (object, (.=))
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import Data.HVect
import qualified Data.HVect as HV
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types (status400)
import qualified Network.Wai as Wai
import Web.HttpApiData (FromHttpApiData, parseHeader, parseQueryParam)
import Web.Spock.Api
import Web.Spock.Api.Document
import Web.Spock.Core

-- | Wire an 'Endpoint' defined using the @Spock-api@ package
defEndpoint ::
  forall p i o m ctx.
  (MonadIO m, HasRep p) =>
  Endpoint p i o ->
  HVectElim p (HVectElim (MaybeToList i) (ActionCtxT ctx m o)) ->
  SpockCtxT ctx m ()
defEndpoint ep handler =
  defEndpointCore (ep, step2)
  where
    step1 :: HVect p -> HVectElim (MaybeToList i) (ActionCtxT ctx m o)
    step1 = HV.uncurry handler

    step2 :: HVect p -> HVect (MaybeToList i) -> ActionCtxT ctx m o
    step2 p = HV.uncurry (step1 p)

-- | Register a documented endpoint. Arguments are path captures, query/header
-- parameters in declaration order, then the JSON body (if any). Scalar
-- duplicates, missing required parameters, and malformed values return 400.
-- Like 'defEndpoint', this uses Spock-core routing; configure browser CSRF
-- protection explicitly if using cookie authentication.
defDocumentedEndpoint :: forall p q i o m ctx. (MonadIO m, HasRep p) =>
  DocumentedEndpoint p q i o ->
  HVectElim p (HVectElim q (HVectElim (MaybeToList i) (ActionCtxT ctx m o))) ->
  SpockCtxT ctx m ()
defDocumentedEndpoint endpoint handler = do
  case validateEndpoint endpoint of
    Left (OpenApiError message) -> liftIO $ ioError $ userError $ T.unpack message
    Right () -> pure ()
  defEndpointCore (de_endpoint endpoint, run)
  where
    run :: HVect p -> HVect (MaybeToList i) -> ActionCtxT ctx m o
    run pathValues bodyValues = do
      req <- request
      values <- case readParameters (de_parameters endpoint) req of
        Left message -> setStatus status400 >> json (object ["error" .= message])
        Right parsed -> pure parsed
      HV.uncurry (HV.uncurry (HV.uncurry handler pathValues) values) bodyValues

defEndpointCore ::
  forall p i o m ctx.
  (MonadIO m, HasRep p) =>
  (Endpoint p i o, HVect p -> HVect (MaybeToList i) -> ActionCtxT ctx m o) ->
  SpockCtxT ctx m ()
defEndpointCore t =
  case t of
    (MethodGet path, handler) ->
      let pf :: HVect p -> ActionCtxT ctx m ()
          pf args =
            do
              r <- handler args HNil
              json r
       in get path (HV.curry pf)
    (MethodPost _ path, handler) ->
      let pf :: HVect p -> ActionCtxT ctx m ()
          pf args =
            do
              req <- jsonBody'
              r <- handler args (req :&: HNil)
              json r
       in post path (HV.curry pf)
    (MethodPut _ path, handler) ->
      let pf :: HVect p -> ActionCtxT ctx m ()
          pf args =
            do
              req <- jsonBody'
              r <- handler args (req :&: HNil)
              json r
       in put path (HV.curry pf)
    (MethodPatch _ path, handler) ->
      let pf :: HVect p -> ActionCtxT ctx m ()
          pf args = do
            req <- jsonBody'
            handler args (req :&: HNil) >>= json
       in patch path (HV.curry pf)
    (MethodDelete path, handler) ->
      let pf :: HVect p -> ActionCtxT ctx m ()
          pf args = handler args HNil >>= json
       in delete path (HV.curry pf)

readParameters :: Parameters q -> Wai.Request -> Either T.Text (HVect q)
readParameters NoParameters _ = Right HNil
readParameters (parameter :> rest) req = (:&:) <$> readParameter parameter <*> readParameters rest req
  where
    readParameter :: Parameter a -> Either T.Text a
    readParameter (QueryParam info) = required "query" info parseQuery (queryValues info)
    readParameter (OptionalQueryParam info) = optional "query" info parseQuery (queryValues info)
    readParameter (QueryList info) = mapM (parseValue "query" info parseQuery) (queryValues info)
    readParameter (HeaderParam info) = required "header" info parseHeader (headerValues info)
    readParameter (OptionalHeaderParam info) = optional "header" info parseHeader (headerValues info)
    queryValues :: ParameterInfo a -> [BS.ByteString]
    queryValues info = [fromMaybe BS.empty value | (name, value) <- Wai.queryString req, name == T.encodeUtf8 (pi_name info)]
    headerValues :: ParameterInfo a -> [BS.ByteString]
    headerValues info = [value | (name, value) <- Wai.requestHeaders req, name == CI.mk (T.encodeUtf8 $ pi_name info)]

parseQuery :: FromHttpApiData a => BS.ByteString -> Either T.Text a
parseQuery value = case T.decodeUtf8' value of
  Left _ -> Left "Invalid UTF-8"
  Right textValue -> parseQueryParam textValue

required :: T.Text -> ParameterInfo a -> (BS.ByteString -> Either T.Text a) -> [BS.ByteString] -> Either T.Text a
required location info parser values = case values of
  [] -> Left $ "Missing " <> location <> " parameter: " <> pi_name info
  [value] -> parseValue location info parser value
  _ -> Left $ "Duplicate " <> location <> " parameter: " <> pi_name info

optional :: T.Text -> ParameterInfo a -> (BS.ByteString -> Either T.Text a) -> [BS.ByteString] -> Either T.Text (Maybe a)
optional _ _ _ [] = Right Nothing
optional location info parser values = Just <$> required location info parser values

parseValue :: T.Text -> ParameterInfo a -> (BS.ByteString -> Either T.Text a) -> BS.ByteString -> Either T.Text a
parseValue location info parser value = case parser value of
  -- Do not include client-supplied values in error responses.
  Left _ -> Left $ "Invalid " <> location <> " parameter: " <> pi_name info
  Right parsed -> Right parsed
