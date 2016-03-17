{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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

#if MIN_VERSION_mtl(2,2,0)
import Control.Monad.Except
#else
import Control.Monad.Error
#endif
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Data.Pool
import Data.Word
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
         -> SpockCfg conn sess st
         -> SpockAllM r conn sess st ()
         -> IO Wai.Middleware
spockAll regIf spockCfg defs =
    do sessionMgr <- createSessionManager sessionCfg
       connectionPool <-
           case poolOrConn of
             PCNoDatabase ->
                 createPool (return ()) (const $ return ()) 5 60 5
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
       spockAllT (spc_maxRequestSize spockCfg) regIf (\m -> runResourceT $ runReaderT (runWebStateT m) internalState)  $
               do defs
                  middleware (sm_middleware sessionMgr)
    where
      sessionCfg = spc_sessionCfg spockCfg
      poolOrConn = spc_database spockCfg
      initialState = spc_initialState spockCfg

-- | Run a raw spock server on a defined port. If you don't need
-- a custom base monad you can just supply 'id' as lift function.
spockAllT :: (MonadIO m, AbstractRouter r, RouteAppliedAction r ~ ActionT m ())
       => Maybe Word64
       -> r
       -> (forall a. m a -> IO a)
       -> SpockAllT r m ()
       -> IO Wai.Middleware
spockAllT = buildMiddleware
