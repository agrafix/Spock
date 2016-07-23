{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Spock
    ( -- * Spock's route definition monad
      spock, SpockM, SpockCtxM
      -- * Safe actions
    , SafeAction (..)
    , safeActionPath
      -- * Core functionality
    , module Web.Spock.Core
    , SpockAction, SpockActionCtx
    , HasSpock(..), SessionManager
    , module Web.Spock.SessionActions
      -- * Accessing internals
    , WebStateM, WebStateT, WebState
    , getSpockHeart, runSpockIO, getSpockPool
    )
where


import Web.Spock.Core
import Web.Spock.Internal.Monad
import Web.Spock.Internal.SessionManager
import Web.Spock.Internal.Types
import Web.Spock.SessionActions

import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Pool
import Data.Monoid
import Web.Routing.SafeRouting hiding (renderRoute)
import qualified Data.Text as T
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

type SpockM conn sess st = SpockCtxM () conn sess st
type SpockCtxM ctx conn sess st = SpockCtxT ctx (WebStateM conn sess st)

-- | Create a spock application using a given db storageLayer and an initial state.
-- Spock works with database libraries that already implement connection pooling and
-- with those that don't come with it out of the box. For more see the 'PoolOrConn' type.
-- Use @runSpock@ to run the app or @spockAsApp@ to create a @Wai.Application@
spock :: SpockCfg conn sess st -> SpockM conn sess st () -> IO Wai.Middleware
spock spockCfg spockAppl =
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
           maxRequestSize = spc_maxRequestSize spockCfg
           coreConfig = defaultSpockConfig { sc_maxRequestSize = maxRequestSize }
       spockConfigT coreConfig (\m -> runResourceT $ runReaderT (runWebStateT m) internalState)  $
           do middleware (sm_middleware sessionMgr)
              hookSafeActions
              spockAppl
    where
      sessionCfg = spc_sessionCfg spockCfg
      poolOrConn = spc_database spockCfg
      initialState = spc_initialState spockCfg

-- | Wire up a safe action: Safe actions are actions that are protected from
-- csrf attacks. Here's a usage example:
--
-- > newtype DeleteUser = DeleteUser Int deriving (Hashable, Typeable, Eq)
-- >
-- > instance SafeAction Connection () () DeleteUser where
-- >    runSafeAction (DeleteUser i) =
-- >       do runQuery $ deleteUserFromDb i
-- >          redirect "/user-list"
-- >
-- > get ("user-details" <//> var) $ \userId ->
-- >   do deleteUrl <- safeActionPath (DeleteUser userId)
-- >      html $ "Click <a href='" <> deleteUrl <> "'>here</a> to delete user!"
--
-- Note that safeActions currently only support GET and POST requests.
--
safeActionPath :: forall conn sess st a.
                  ( SafeAction conn sess st a
                  , HasSpock(SpockAction conn sess st)
                  )
               => a
               -> SpockAction conn sess st T.Text
safeActionPath safeAction =
    do mgr <- getSessMgr
       hash <- sm_addSafeAction mgr (PackedSafeAction safeAction)
       return $ "/h/" <> hash

hookSafeActions :: SpockM conn sess st ()
hookSafeActions =
    getpost (static "h" </> var) run
    where
      run h =
          do mgr <- getSessMgr
             mAction <- sm_lookupSafeAction mgr h
             case mAction of
               Nothing ->
                   do setStatus Http.status404
                      text "File not found"
               Just p@(PackedSafeAction action) ->
                   do runSafeAction action
                      sm_removeSafeAction mgr p
