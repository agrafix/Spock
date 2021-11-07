{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Spock.Api.Server (defEndpoint) where

import Control.Monad.Trans
import Data.HVect
import qualified Data.HVect as HV
import Web.Spock.Api
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
