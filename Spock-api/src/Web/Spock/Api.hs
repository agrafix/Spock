{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Web.Spock.Api
    ( Endpoint(..)
    , Proxy(..)
    , MaybeToList
    , (<//>), var, Path(..), renderRoute
    , Generic, ToJSON, FromJSON, NFData, Typeable
    )
where

import Control.DeepSeq
import Data.Aeson
import Data.HVect
import Data.Proxy
import Data.Typeable
import GHC.Generics
import Web.Routing.Combinators

(<//>) :: Path as 'Open -> Path bs ps -> Path (Append as bs) ps
(<//>) = (</>)

-- | Describes an endpoint with path parameters, an optional json body and a json response
data Endpoint (p :: [*]) (i :: Maybe *) (o :: *) where
    MethodGet :: (ToJSON o, FromJSON o) => Path p 'Open -> Endpoint p 'Nothing o
    MethodPost ::
        (ToJSON i, FromJSON i, ToJSON o, FromJSON o)
        => Proxy (i -> o) -> Path p 'Open -> Endpoint p ('Just i) o
    MethodPut ::
        (ToJSON i, FromJSON i, ToJSON o, FromJSON o)
        => Proxy (i -> o) -> Path p 'Open -> Endpoint p ('Just i) o
