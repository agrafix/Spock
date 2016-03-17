{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Web.Spock.Internal.Core
    ( SpockAllT
    , spockAllT
    , middleware
    , hookRoute
    , hookAny
    , subcomponent
    )
where

import Web.Spock.Internal.Wire

#if MIN_VERSION_mtl(2,2,0)
import Control.Monad.Except
#else
import Control.Monad.Error
#endif
import Data.Word
import Prelude hiding (head)
import Web.Routing.AbstractRouter
import qualified Network.Wai as Wai

-- | Run a raw spock server on a defined port. If you don't need
-- a custom base monad you can just supply 'id' as lift function.
spockAllT :: (MonadIO m, AbstractRouter r, RouteAppliedAction r ~ ActionT m ())
       => Maybe Word64
       -> r
       -> (forall a. m a -> IO a)
       -> SpockAllT r m ()
       -> IO Wai.Middleware
spockAllT = buildMiddleware
