{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
module Web.Spock.Internal.Core
    ( SpockAllT
    , spockAllT
    , middleware
    , hookRoute
    , subcomponent
    )
where

import Web.Spock.Internal.Wire

import Control.Monad.Error
import Prelude hiding (head)
import Web.Routing.AbstractRouter
import qualified Network.Wai.Handler.Warp as Warp

-- | Run a raw spock server on a defined port. If you don't need
-- a custom base monad you can just supply 'id' as lift function.
spockAllT :: forall (path :: k -> *) (action :: k -> *)  reg m. (IsPath path, MonadIO m)
       => SpockRegistryIf path action m reg
       -> Warp.Port
       -> (forall a. m a -> IO a)
       -> SpockAllT path action reg m ()
       -> IO ()
spockAllT registryIf port liftSpock routeDefs =
    do spockApp <- buildApp registryIf liftSpock routeDefs
       putStrLn $ "Spock is up and running on port " ++ show port
       Warp.run port spockApp
