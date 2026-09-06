{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Build an application with typed routes, sessions, and a database pool.
--
-- = Reading requests and sending responses
--
-- Request and response helpers are reexported from "Web.Spock.Action" in the
-- @Spock-core@ package. Follow that module link for the complete action API:
--
-- * 'param' and 'param'': parse query or form parameters. Values captured by
--   'var' in a route are passed directly to its handler instead.
-- * 'paramsGet', 'paramsPost', and 'params': list request parameters.
-- * 'jsonBody', 'jsonBody'', and 'body': read JSON or raw request bytes.
-- * 'header', 'rawHeader', and 'cookies': inspect request metadata.
-- * 'filesMulti': read uploaded files, including repeated upload fields.
-- * 'setStatus' and 'setHeader': prepare response metadata before sending it.
-- * 'text', 'html', 'json', 'file', and 'lazyBytes': send a response and finish
--   the current action.
--
-- The <https://www.spock.li/reference/ API reference> links the current
-- versions of Spock, Spock-core, the typed API packages, and session adapters.
module Web.Spock
  ( -- * Launching Spock
    runSpock,
    runSpockNoBanner,
    spockAsApp,

    -- * Spock's route definition monad
    spock,
    SpockM,
    SpockCtxM,

    -- * Defining routes
    Path,
    root,
    Var,
    AltVar (..),
    var,
    static,
    (<//>),
    wildcard,

    -- * Rendering routes
    renderRoute,

    -- * Hooking routes
    prehook,
    RouteSpec,
    get,
    post,
    getpost,
    head,
    put,
    delete,
    patch,
    hookRoute,
    hookRouteCustom,
    hookAny,
    hookAnyCustom,
    hookRouteAll,
    hookAnyAll,
    C.StdMethod (..),

    -- * Adding Wai.Middleware
    middleware,

    -- * Actions
    SpockAction,
    SpockActionCtx,
    module Web.Spock.Action,
    HasSpock (..),
    SessionManager,
    module Web.Spock.SessionActions,
    getCsrfToken,
    getClientCsrfToken,
    csrfCheck,

    -- * Accessing internals
    WebStateM,
    WebStateT,
    WebState,
    getSpockHeart,
    runSpockIO,
    getSpockPool,
  )
where

import Control.Applicative
import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import qualified Data.HVect as HV
import Data.Pool
import qualified Data.Text as T
import qualified Data.Vault.Lazy as V
import Network.HTTP.Types.Status (status403)
import qualified Network.Wai as Wai
import Web.Spock.Action
import Web.Spock.Core hiding
  ( delete,
    get,
    getpost,
    head,
    hookAny,
    hookAny',
    hookAnyAll,
    hookAnyCustom,
    hookRoute,
    hookRoute',
    hookRouteAll,
    hookRouteCustom,
    patch,
    post,
    put,
  )
import qualified Web.Spock.Core as C
import Web.Spock.Internal.Monad
import Web.Spock.Internal.SessionManager
import Web.Spock.Internal.Types
import Web.Spock.Routing
import Web.Spock.SessionActions
import Prelude hiding (head)

-- | Register routes and middleware when the application starts. The handler
-- passed to a route runs later, once per matching request, in 'SpockAction'.
--
-- @conn@ is a database connection, @sess@ is one visitor's session value, and
-- @st@ is application-wide state. The final result parameter is usually @()@.
-- This is 'SpockCtxM' with an empty request context. Use 'SpockCtxM' inside a
-- 'prehook' that supplies a typed context.
type SpockM conn sess st = SpockCtxM () conn sess st

-- | Route registration with handlers that receive context @ctx@. A 'prehook'
-- produces this context per request; 'getContext' reads it in the handler.
-- Context belongs to the selected request and hook scope, while @st@ is shared
-- by the entire application and @sess@ belongs to a visitor's session.
--
-- The underlying monad is 'WebStateM': @lift helper@ runs a shared-state or
-- database helper during registration. Inside a handler, the same expression
-- runs it for that request. Use @liftIO@ for an ordinary IO operation.
type SpockCtxM ctx conn sess st = SpockCtxT ctx (WebStateM conn sess st)

-- | Create a spock application using a given db storageLayer and an initial state.
-- Spock works with database libraries that already implement connection pooling and
-- with those that don't come with it out of the box. For more see the 'PoolOrConn' type.
-- Use @runSpock@ to run the app or @spockAsApp@ to create a @Wai.Application@
spock :: forall conn sess st. SpockCfg conn sess st -> SpockM conn sess st () -> IO Wai.Middleware
spock spockCfg spockAppl =
  do
    when (sc_sessionMode sessionCfg == SessionsDisabled && spc_csrfProtection spockCfg) $
      throwIO CsrfRequiresSessions
    connectionPool <-
      case poolOrConn of
        PCNoDatabase ->
          newPool $ setNumStripes (Just 5) $ defaultPoolConfig (return ()) (const $ return ()) 60 25
        PCPool p ->
          return p
        PCConn cb ->
          let pc = cb_poolConfiguration cb
           in newPool $
                setNumStripes (Just $ pc_stripes pc) $
                  defaultPoolConfig
                    (cb_createConn cb)
                    (cb_destroyConn cb)
                    (realToFrac $ pc_keepOpenTime pc)
                    (pc_stripes pc * pc_resPerStripe pc)
    internalState <-
      WebState connectionPool
        <$> ( createSessionManager sessionCfg $
                SessionIf
                  { si_queryVault = queryVault,
                    si_modifyVault = modifyVault,
                    si_setRawMultiHeader = setRawMultiHeader,
                    si_vaultKey = V.newKey
                  }
            )
        <*> pure initialState
        <*> pure spockCfg
    let coreConfig =
          defaultSpockConfig
            { sc_maxRequestSize = spc_maxRequestSize spockCfg,
              sc_errorHandler = spc_errorHandler spockCfg,
              sc_logError = spc_logError spockCfg,
              sc_logging = spc_logging spockCfg
            }
    spockConfigT coreConfig (\m -> runResourceT $ runReaderT (runWebStateT m) internalState) $
      do
        middleware (sm_middleware $ web_sessionMgr internalState)
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
  do
    cfg <- getSpockCfg
    mHeader <- header (spc_csrfHeaderName cfg)
    mParam <- param (spc_csrfPostName cfg)
    pure (mHeader <|> mParam)
{-# INLINE getClientCsrfToken #-}

-- | Check that the client sent a valid CSRF token. You should not need to call this
-- manually in non GET requests if 'spc_csrfProtection' is turned on.
csrfCheck :: SpockActionCtx ctx conn sess st ()
csrfCheck =
  do
    csrf <- getCsrfToken
    clientCsrf <- getClientCsrfToken
    case clientCsrf of
      Nothing -> abort
      Just csrfVal
        | csrfVal == csrf -> pure ()
        | otherwise -> abort
  where
    abort =
      do
        setStatus status403
        text "Broken/Missing CSRF Token"
{-# INLINE csrfCheck #-}

type RouteMonad t ctx conn sess st a =
  (Monad (t ctx (WebStateM conn sess st)), RouteM t) => t ctx (WebStateM conn sess st) a

type RouteSpec t xs ps ctx conn sess st =
  Path xs ps -> HV.HVectElim xs (SpockActionCtx ctx conn sess st ()) -> RouteMonad t ctx conn sess st ()

-- | Specify an action that will be run when a standard HTTP verb and the given route match
hookRoute :: HV.HasRep xs => StdMethod -> RouteSpec t xs ps ctx conn sess st
hookRoute m = hookRoute' (MethodStandard . HttpMethod $ m)

-- | Specify an action that will be run regardless of the HTTP verb
hookRouteAll :: HV.HasRep xs => RouteSpec t xs ps ctx conn sess st
hookRouteAll = hookRoute' MethodAny

-- | Specify an action that will be run when the HTTP verb 'GET' and the given route match
get :: HV.HasRep xs => RouteSpec t xs ps ctx conn sess st
get = hookRoute GET

-- | Specify an action that will be run when the HTTP verb 'POST' and the given route match
post :: HV.HasRep xs => RouteSpec t xs ps ctx conn sess st
post = hookRoute POST

-- | Specify an action that will be run when the HTTP verb 'GET'/'POST' and the given route match
getpost :: HV.HasRep xs => RouteSpec t xs ps ctx conn sess st
getpost r a = hookRoute POST r a >> hookRoute GET r a

-- | Specify an action that will be run when the HTTP verb 'HEAD' and the given route match
head :: HV.HasRep xs => RouteSpec t xs ps ctx conn sess st
head = hookRoute HEAD

-- | Specify an action that will be run when the HTTP verb 'PUT' and the given route match
put :: HV.HasRep xs => RouteSpec t xs ps ctx conn sess st
put = hookRoute PUT

-- | Specify an action that will be run when the HTTP verb 'DELETE' and the given route match
delete :: HV.HasRep xs => RouteSpec t xs ps ctx conn sess st
delete = hookRoute DELETE

-- | Specify an action that will be run when the HTTP verb 'PATCH' and the given route match
patch :: HV.HasRep xs => RouteSpec t xs ps ctx conn sess st
patch = hookRoute PATCH

-- | Specify an action that will be run when a custom HTTP verb and the given route match
hookRouteCustom :: HV.HasRep xs => T.Text -> RouteSpec t xs ps ctx conn sess st
hookRouteCustom t = hookRoute' (MethodCustom t)

-- | Specify an action that will be run when a standard HTTP verb matches but no defined route matches.
-- The full path is passed as an argument
hookAny :: StdMethod -> ([T.Text] -> SpockActionCtx ctx conn sess st ()) -> RouteMonad t ctx conn sess st ()
hookAny m = hookAny' (MethodStandard . HttpMethod $ m)

-- | Specify an action that will be run regardless of the HTTP verb and no defined route matches.
-- The full path is passed as an argument
hookAnyAll :: ([T.Text] -> SpockActionCtx ctx conn sess st ()) -> RouteMonad t ctx conn sess st ()
hookAnyAll = hookAny' MethodAny

-- | Specify an action that will be run when a custom HTTP verb matches but no defined route matches.
-- The full path is passed as an argument
hookAnyCustom :: T.Text -> ([T.Text] -> SpockActionCtx ctx conn sess st ()) -> RouteMonad t ctx conn sess st ()
hookAnyCustom t = hookAny' (MethodCustom t)

-- | Specify an action that will be run when a HTTP verb matches but no defined route matches.
-- The full path is passed as an argument
hookAny' :: SpockMethod -> ([T.Text] -> SpockActionCtx ctx conn sess st ()) -> RouteMonad t ctx conn sess st ()
hookAny' m action =
  C.hookAny' m $ \t -> csrfCheckIfEnabled >> action t

-- | Specify an action that will be run when a HTTP verb and the given route match
hookRoute' ::
  forall t xs ps ctx conn sess st.
  (HV.HasRep xs) =>
  SpockMethod ->
  RouteSpec t xs ps ctx conn sess st
hookRoute' m path action =
  let checkedAction :: HV.HVect xs -> SpockActionCtx ctx conn sess st ()
      checkedAction args = csrfCheckIfEnabled >> HV.uncurry action args
   in C.hookRoute' m path (HV.curry checkedAction)

csrfCheckIfEnabled :: SpockActionCtx ctx conn sess st ()
csrfCheckIfEnabled =
  do
    method <- reqMethod
    when (shouldCheckCsrf method) $
      do
        cfg <- getSpockCfg
        when (spc_csrfProtection cfg) csrfCheck

-- Check the request method, including when the route accepts every method.
shouldCheckCsrf :: SpockMethod -> Bool
shouldCheckCsrf m =
  case m of
    MethodStandard (HttpMethod GET) -> False
    MethodStandard (HttpMethod HEAD) -> False
    MethodStandard (HttpMethod OPTIONS) -> False
    _ -> True
