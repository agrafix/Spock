{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Web.Spock.Api.Client (callEndpoint, callEndpoint') where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import Data.HVect
import qualified Data.HVect as HV
import qualified Data.JSString as J
import qualified Data.JSString.Text as J
import qualified Data.Text.Encoding as T
import JavaScript.Web.XMLHttpRequest
import Web.Spock.Api

type Header = (J.JSString, J.JSString)

-- | Call an 'Endpoint' defined using the @Spock-api@ package passing extra headers
callEndpoint' ::
  forall p i o.
  (HasRep (MaybeToList i), HasRep p) =>
  Endpoint p i o ->
  [Header] ->
  HVectElim p (HVectElim (MaybeToList i) (IO (Maybe o)))
callEndpoint' ep extraHeaders =
  HV.curry $ \hv -> HV.curry (callEndpointCore' ep extraHeaders hv)

-- | Call an 'Endpoint' defined using the @Spock-api@ package
callEndpoint ::
  forall p i o.
  (HasRep (MaybeToList i), HasRep p) =>
  Endpoint p i o ->
  HVectElim p (HVectElim (MaybeToList i) (IO (Maybe o)))
callEndpoint ep = callEndpoint' ep []

data EndpointCall p i o = EndpointCall
  { epc_point :: !(Endpoint p i o),
    epc_headers :: ![Header],
    epc_params :: !(HVect p),
    epc_body :: !(HVect (MaybeToList i))
  }

callEndpointCore' ::
  forall p i o.
  Endpoint p i o ->
  [Header] ->
  HVect p ->
  HVect (MaybeToList i) ->
  IO (Maybe o)
callEndpointCore' ep hdrs hv b = callEndpointCore (EndpointCall ep hdrs hv b)

callEndpointCore :: forall p i o. EndpointCall p i o -> IO (Maybe o)
callEndpointCore call =
  case call of
    EndpointCall (MethodPost Proxy path) hdrs params (body :&: HNil) ->
      do
        let rt = J.textToJSString $ renderRoute path params
            bodyText = J.textToJSString $ T.decodeUtf8 $ BSL.toStrict $ A.encode body
            req =
              Request
                { reqMethod = POST,
                  reqURI = rt,
                  reqLogin = Nothing,
                  reqHeaders = (("Content-Type", "application/json;charset=UTF-8") : hdrs),
                  reqWithCredentials = False,
                  reqData = StringData bodyText
                }
        runJsonReq req
    EndpointCall (MethodPut Proxy path) hdrs params (body :&: HNil) ->
      do
        let rt = J.textToJSString $ renderRoute path params
            bodyText = J.textToJSString $ T.decodeUtf8 $ BSL.toStrict $ A.encode body
            req =
              Request
                { reqMethod = PUT,
                  reqURI = rt,
                  reqLogin = Nothing,
                  reqHeaders = (("Content-Type", "application/json;charset=UTF-8") : hdrs),
                  reqWithCredentials = False,
                  reqData = StringData bodyText
                }
        runJsonReq req
    EndpointCall (MethodGet path) hdrs params HNil ->
      do
        let rt = J.textToJSString $ renderRoute path params
            req =
              Request
                { reqMethod = GET,
                  reqURI = rt,
                  reqLogin = Nothing,
                  reqHeaders = hdrs,
                  reqWithCredentials = False,
                  reqData = NoData
                }
        runJsonReq req

runJsonReq :: A.FromJSON o => Request -> IO (Maybe o)
runJsonReq req =
  do
    response <- xhrText req
    case (status response, contents response) of
      (200, Just txt) ->
        do
          let res = A.eitherDecodeStrict' (T.encodeUtf8 txt)
          case res of
            Left errMsg ->
              do
                putStrLn errMsg
                pure Nothing
            Right val ->
              pure (Just val)
      _ -> pure Nothing
