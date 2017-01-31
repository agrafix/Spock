{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Spock
    ( -- * Launching Spock
      runSpock, runSpockNoBanner, spockAsApp
      -- * Spock's route definition monad
    , spock, SpockM, SpockCtxM
      -- * Defining routes
    , Path, root, Var, var, static, (<//>), wildcard
      -- * Rendering routes
    , renderRoute
      -- * Hooking routes
    , subcomponent, prehook
    , RouteSpec
    , get, post, getpost, head, put, delete, patch, hookRoute
    , hookRouteCustom, hookAny, hookAnyCustom
    , C.StdMethod (..)
      -- * Adding Wai.Middleware
    , middleware
      -- * Actions
    , SpockAction, SpockActionCtx
    , module Web.Spock.Action
    , HasSpock(..), SessionManager
    , module Web.Spock.SessionActions
    , getCsrfToken, getClientCsrfToken, csrfCheck
      -- * Accessing internals
    , WebStateM, WebStateT, WebState
    , getSpockHeart, runSpockIO, getSpockPool
    )
where

import Web.Spock.Action
import Web.Spock.Core hiding
       ( hookRoute', hookAny'
       , get, post, getpost, head, put, delete, patch, hookRoute
       , hookRouteCustom, hookAny, hookAnyCustom
       )
import Web.Spock.Internal.Monad
import Web.Spock.Internal.SessionManager
import Web.Spock.Internal.Types
import Web.Spock.SessionActions
import qualified Web.Spock.Core as C

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Pool
import Network.HTTP.Types.Status (status403)
import Prelude hiding (head)
import qualified Data.HVect as HV
import qualified Data.Text as T
import qualified Data.Vault.Lazy as V
import qualified Network.Wai as Wai


type SpockM conn sess st = SpockCtxM () conn sess st
type SpockCtxM ctx conn sess st = SpockCtxT ctx (WebStateM conn sess st)

-- | Create a spock application using a given db storageLayer and an initial state.
-- Spock works with database libraries that already implement connection pooling and
-- with those that don't come with it out of the box. For more see the 'PoolOrConn' type.
-- Use @runSpock@ to run the app or @spockAsApp@ to create a @Wai.Application@
spock :: forall conn sess st. SpockCfg conn sess st -> SpockM conn sess st () -> IO Wai.Middleware
spock spockCfg spockAppl =
    do connectionPool <-
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
       internalState <-
           WebState
           <$> pure connectionPool
           <*> (createSessionManager sessionCfg $
                   SessionIf
                   { si_queryVault = queryVault
                   , si_modifyVault = modifyVault
                   , si_setRawMultiHeader = setRawMultiHeader
                   , si_vaultKey = V.newKey
                   }
               )
           <*> pure initialState
           <*> pure spockCfg
       let coreConfig =
               defaultSpockConfig
               { sc_maxRequestSize = spc_maxRequestSize spockCfg
               , sc_errorHandler = spc_errorHandler spockCfg
               }
       spockConfigT coreConfig (\m -> runResourceT $ runReaderT (runWebStateT m) internalState)  $
           do middleware (sm_middleware $ web_sessionMgr internalState)
              spockAppl
    where
        sessionCfg = spc_sessionCfg spockCfg
        poolOrConn = spc_database spockCfg
        initialState = spc_initialState spockCfg

-- | Get the CSRF token for the current user. This token must be sent on all non
-- GET requests via a post parameter or HTTP-Header if 'spc_csrfProtection' is turned on.
-- See configuration 'SpockCfg' documentation for more information
getCsrfToken :: SpockActionCtx ctx conn sess st T.Text
getCsrfToken = runInContext () $ sm_getCsrfToken =<< getSessMgr
{-# INLINE getCsrfToken #-}

-- | Get the CSRF token sent by the client. You should not need to call this
-- manually if 'spc_csrfProtection' is turned on.
getClientCsrfToken :: SpockActionCtx ctx conn sess st (Maybe T.Text)
getClientCsrfToken =
    do cfg <- getSpockCfg
       mHeader <- header (spc_csrfHeaderName cfg)
       mParam <- param (spc_csrfPostName cfg)
       pure (mHeader <|> mParam)
{-# INLINE getClientCsrfToken #-}

-- | Check that the client sent a valid CSRF token. You should not need to call this
-- manually if 'spc_csrfProtection' is turned on.
csrfCheck :: SpockActionCtx ctx conn sess st ()
csrfCheck =
    do csrf <- getCsrfToken
       clientCsrf <- getClientCsrfToken
       case clientCsrf of
         Nothing -> abort
         Just csrfVal
           | csrfVal == csrf -> pure ()
           | otherwise -> abort
   where
       abort =
         do setStatus status403
            text "Broken/Missing CSRF Token"
{-# INLINE csrfCheck #-}

type RouteSpec xs ps ctx conn sess st =
    Path xs ps -> HV.HVectElim xs (SpockActionCtx ctx conn sess st ()) -> SpockCtxM ctx conn sess st ()

-- | Specify an action that will be run when a standard HTTP verb and the given route match
hookRoute :: HV.HasRep xs => StdMethod -> RouteSpec xs ps ctx conn sess st
hookRoute = hookRoute' . MethodStandard . HttpMethod

-- | Specify an action that will be run when the HTTP verb 'GET' and the given route match
get :: HV.HasRep xs => RouteSpec xs ps ctx conn sess st
get = hookRoute GET

-- | Specify an action that will be run when the HTTP verb 'POST' and the given route match
post :: HV.HasRep xs => RouteSpec xs ps ctx conn sess st
post = hookRoute POST

-- | Specify an action that will be run when the HTTP verb 'GET'/'POST' and the given route match
getpost :: HV.HasRep xs => RouteSpec xs ps ctx conn sess st
getpost r a = hookRoute POST r a >> hookRoute GET r a

-- | Specify an action that will be run when the HTTP verb 'HEAD' and the given route match
head :: HV.HasRep xs => RouteSpec xs ps ctx conn sess st
head = hookRoute HEAD

-- | Specify an action that will be run when the HTTP verb 'PUT' and the given route match
put :: HV.HasRep xs => RouteSpec xs ps ctx conn sess st
put = hookRoute PUT

-- | Specify an action that will be run when the HTTP verb 'DELETE' and the given route match
delete :: HV.HasRep xs => RouteSpec xs ps ctx conn sess st
delete = hookRoute DELETE

-- | Specify an action that will be run when the HTTP verb 'PATCH' and the given route match
patch :: HV.HasRep xs => RouteSpec xs ps ctx conn sess st
patch = hookRoute PATCH

-- | Specify an action that will be run when a custom HTTP verb and the given route match
hookRouteCustom :: HV.HasRep xs => T.Text -> RouteSpec xs ps ctx conn sess st
hookRouteCustom = hookRoute' . MethodCustom

-- | Specify an action that will be run when a standard HTTP verb matches but no defined route matches.
-- The full path is passed as an argument
hookAny :: StdMethod -> ([T.Text] -> SpockActionCtx ctx conn sess st ()) -> SpockCtxM ctx conn sess st ()

hookAny = hookAny' . MethodStandard . HttpMethod

-- | Specify an action that will be run when a custom HTTP verb matches but no defined route matches.
-- The full path is passed as an argument
hookAnyCustom :: T.Text -> ([T.Text] -> SpockActionCtx ctx conn sess st ()) -> SpockCtxM ctx conn sess st ()

hookAnyCustom = hookAny' . MethodCustom

-- | Specify an action that will be run when a HTTP verb matches but no defined route matches.
-- The full path is passed as an argument
hookAny' :: SpockMethod -> ([T.Text] -> SpockActionCtx ctx conn sess st ()) -> SpockCtxM ctx conn sess st ()
hookAny' m action =
    getSpockCfg >>= \cfg ->
    C.hookAny' m $ \t ->
    case m of
      MethodStandard (HttpMethod stdMethod)
          | shouldCheckCsrf stdMethod && spc_csrfProtection cfg -> csrfCheck >> action t
      _ -> action t

-- | Specify an action that will be run when a HTTP verb and the given route match
hookRoute' ::
    forall xs ps ctx conn sess st.
    (HV.HasRep xs)
    => SpockMethod
    -> RouteSpec xs ps ctx conn sess st
hookRoute' m path action =
    do cfg <- getSpockCfg
       checkedAction <-
           case m of
               MethodStandard (HttpMethod stdMethod)
                   | shouldCheckCsrf stdMethod && spc_csrfProtection cfg ->
                         do let unpackedAction :: HV.HVect xs -> SpockActionCtx ctx conn sess st ()
                                unpackedAction args =
                                    csrfCheck >> HV.uncurry action args
                            pure $ HV.curry unpackedAction
               _ -> pure action
       C.hookRoute' m path checkedAction

shouldCheckCsrf :: StdMethod -> Bool
shouldCheckCsrf m =
    case m of
      GET -> False
      HEAD -> False
      OPTIONS -> False
      _ -> True
