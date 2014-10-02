{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Web.Spock.Internal.Wrapper where

import Web.Spock.Internal.Core
import Web.Spock.Internal.Wire
import Web.Spock.Internal.SessionManager
import Web.Spock.Internal.Types

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Data.Pool
import Web.Routing.AbstractRouter

-- | Run a spock application using the warp server, a given db storageLayer and an initial state.
-- Spock works with database libraries that already implement connection pooling and
-- with those that don't come with it out of the box. For more see the 'PoolOrConn' type.
spockAll :: forall r conn sess st.
            ( AbstractRouter r
            , RouteAppliedAction r ~ ActionT (WebStateM conn sess st) ()
            )
         => r
         -> Int
         -> SessionCfg sess
         -> PoolOrConn conn
         -> st
         -> SpockAllM r conn sess st ()
         -> IO ()
spockAll regIf port sessionCfg poolOrConn initialState defs =
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
       spockAllT regIf port (\m -> runResourceT $ runReaderT (runWebStateM m) internalState) $
               do defs
                  middleware (sm_middleware sessionMgr)
