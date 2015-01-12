{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Web.Spock.Internal.Core
    ( SpockAllT
    , spockAll
    , spockAllT
    , middleware
    , hookRoute
    , hookAny
    , subcomponent
    )
where

import Web.Spock.Internal.SessionManager
import Web.Spock.Internal.Types
import Web.Spock.Internal.Wire

import Control.Monad.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Data.Pool
import Prelude hiding (head)
import Web.Routing.AbstractRouter
import qualified Network.Wai as Wai

-- | Run a spock application using the warp server, a given db storageLayer and an initial state.
-- Spock works with database libraries that already implement connection pooling and
-- with those that don't come with it out of the box. For more see the 'PoolOrConn' type.
spockAll :: forall r conn sess st.
            ( AbstractRouter r
            , RouteAppliedAction r ~ ActionT (WebStateM conn sess st) ()
            )
         => r
         -> SessionCfg sess
         -> PoolOrConn conn
         -> st
         -> SpockAllM r conn sess st ()
         -> IO Wai.Middleware
spockAll regIf sessionCfg poolOrConn initialState defs =
    do sessionMgr <- createSessionManager sessionCfg
       connectionPool <-
           case poolOrConn of
             PCPool p ->
                 return p
             PCConn cb ->
                 let pc = cb_poolConfiguration cb
                 in createPool (cb_createConn cb) (cb_destroyConn cb)
                        (pc_stripes pc) (pc_keepOpenTime pc)
                        (pc_resPerStripe pc)
       let internalState =
               WebState
               { web_dbConn = connectionPool
               , web_sessionMgr = sessionMgr
               , web_state = initialState
               }
       spockAllT regIf (\m -> runResourceT $ runReaderT (runWebStateM m) internalState) $
               do defs
                  middleware (sm_middleware sessionMgr)

-- | Run a raw spock server on a defined port. If you don't need
-- a custom base monad you can just supply 'id' as lift function.
spockAllT :: (MonadIO m, AbstractRouter r, RouteAppliedAction r ~ ActionT m ())
       => r
       -> (forall a. m a -> IO a)
       -> SpockAllT r m ()
       -> IO Wai.Middleware
spockAllT registryIf liftSpock routeDefs =
    buildMiddleware registryIf liftSpock routeDefs
