{-# LANGUAGE DataKinds #-}
module Web.Spock.Routing where

import Web.Spock.Action
import Web.Spock.Internal.Wire (SpockMethod(..))

import Control.Monad.Trans
import Data.HVect hiding (head)
import Web.Routing.Combinators
import qualified Data.Text as T
import qualified Network.Wai as Wai

class RouteM t where
    addMiddleware :: Monad m => Wai.Middleware -> t ctx m ()
    withPrehook :: MonadIO m => ActionCtxT ctx m ctx' -> t ctx' m () -> t ctx m ()
    wireAny :: Monad m => SpockMethod -> ([T.Text] -> ActionCtxT ctx m ()) -> t ctx m ()
    wireRoute ::
        (Monad m, HasRep xs)
        => SpockMethod -> Path xs ps -> HVectElim xs (ActionCtxT ctx m ()) -> t ctx m ()
