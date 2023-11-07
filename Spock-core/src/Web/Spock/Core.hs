{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Spock.Core
  ( -- * Lauching Spock
    runSpock,
    runSpockNoBanner,
    spockAsApp,

    -- * Spock's route definition monad
    spockT,
    spockConfigT,
    SpockT,
    SpockCtxT,

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
    Http.StdMethod (..),

    -- * Adding Wai.Middleware
    middleware,

    -- * Actions
    module Web.Spock.Action,

    -- * Config
    SpockConfig (..),
    defaultSpockConfig,

    -- * Internals
    hookRoute',
    hookAny',
    SpockMethod (..),
    W.HttpMethod (..),
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.Foldable (forM_)
import Data.HVect hiding (head)
import qualified Data.Text as T
import qualified Network.HTTP.Types as Http
import Network.HTTP.Types.Method
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.IO
import Web.HttpApiData
import Web.Routing.Combinators hiding (renderRoute)
import qualified Web.Routing.Combinators as COMB
import Web.Routing.Router (swapMonad)
import qualified Web.Routing.Router as AR
import Web.Routing.SafeRouting
import Web.Spock.Action
import Web.Spock.Internal.Config
import Web.Spock.Internal.Wire (SpockMethod (..))
import qualified Web.Spock.Internal.Wire as W
import Web.Spock.Routing
import Prelude hiding (curry, head, uncurry)

type SpockT = SpockCtxT ()

newtype LiftHooked ctx m = LiftHooked {unLiftHooked :: forall a. ActionCtxT ctx m a -> ActionCtxT () m a}

injectHook :: LiftHooked ctx m -> (forall a. ActionCtxT ctx' m a -> ActionCtxT ctx m a) -> LiftHooked ctx' m
injectHook (LiftHooked baseHook) nextHook =
  LiftHooked $ baseHook . nextHook

newtype SpockCtxT ctx m a = SpockCtxT
  { runSpockT :: W.SpockAllT m (ReaderT (LiftHooked ctx m) m) a
  }
  deriving (Monad, Functor, Applicative, MonadIO)

instance MonadTrans (SpockCtxT ctx) where
  lift = SpockCtxT . lift . lift

instance RouteM SpockCtxT where
  addMiddleware = SpockCtxT . AR.middleware
  wireAny m action =
    SpockCtxT $
      do
        hookLift <- lift $ asks (\x -> unLiftHooked x)
        case m of
          MethodAny ->
            do
              forM_ allStdMethods $ \mReg ->
                AR.hookAny mReg (hookLift . action)
              AR.hookAnyMethod (hookLift . action)
          _ -> AR.hookAny m (hookLift . action)
  withPrehook = withPrehookImpl
  wireRoute = wireRouteImpl

withPrehookImpl :: forall m ctx ctx'. MonadIO m => ActionCtxT ctx m ctx' -> SpockCtxT ctx' m () -> SpockCtxT ctx m ()
withPrehookImpl hook (SpockCtxT hookBody) =
  SpockCtxT $
    do
      prevHook <- lift ask
      let newHook :: ActionCtxT ctx' m a -> ActionCtxT ctx m a
          newHook act =
            do
              newCtx <- hook
              runInContext newCtx act
          hookLift :: forall a. ReaderT (LiftHooked ctx' m) m a -> ReaderT (LiftHooked ctx m) m a
          hookLift a =
            lift $ runReaderT a (injectHook prevHook newHook)
      swapMonad hookLift hookBody

wireRouteImpl :: forall xs ctx m ps. (HasRep xs, Monad m) => SpockMethod -> Path xs ps -> HVectElim xs (ActionCtxT ctx m ()) -> SpockCtxT ctx m ()
wireRouteImpl m path action =
  SpockCtxT $
    do
      hookLift <- lift $ asks (\x -> unLiftHooked x)
      let actionPacker :: HVectElim xs (ActionCtxT ctx m ()) -> HVect xs -> ActionCtxT () m ()
          actionPacker act captures = hookLift (uncurry act captures)
      case m of
        MethodAny ->
          do
            forM_ allStdMethods $ \mReg ->
              AR.hookRoute mReg (toInternalPath path) (HVectElim' $ curry $ actionPacker action)
            AR.hookRouteAnyMethod (toInternalPath path) (HVectElim' $ curry $ actionPacker action)
        _ -> AR.hookRoute m (toInternalPath path) (HVectElim' $ curry $ actionPacker action)

allStdMethods :: [SpockMethod]
allStdMethods = MethodStandard <$> [minBound .. maxBound]

-- | Run a Spock application. Basically just a wrapper around 'Warp.run'.
runSpock :: Warp.Port -> IO Wai.Middleware -> IO ()
runSpock port mw =
  do
    hPutStrLn stderr ("Spock is running on port " ++ show port)
    app <- spockAsApp mw
    Warp.run port app

-- | Like 'runSpock', but does not display the banner "Spock is running on port XXX" on stdout.
runSpockNoBanner :: Warp.Port -> IO Wai.Middleware -> IO ()
runSpockNoBanner port mw =
  do
    app <- spockAsApp mw
    Warp.run port app

-- | Convert a middleware to an application. All failing requests will
-- result in a 404 page
spockAsApp :: IO Wai.Middleware -> IO Wai.Application
spockAsApp = fmap W.middlewareToApp

-- | Create a raw spock application with custom underlying monad
-- Use 'runSpock' to run the app or 'spockAsApp' to create a @Wai.Application@
-- The first argument is request size limit in bytes. Set to 'Nothing' to disable.
spockT ::
  (MonadIO m) =>
  (forall a. m a -> IO a) ->
  SpockT m () ->
  IO Wai.Middleware
spockT = spockConfigT defaultSpockConfig

-- | Like 'spockT', but with additional configuration for request size and error
-- handlers passed as first parameter.
spockConfigT ::
  forall m.
  MonadIO m =>
  SpockConfig ->
  (forall a. m a -> IO a) ->
  SpockT m () ->
  IO Wai.Middleware
spockConfigT (SpockConfig maxRequestSize errorAction logError) liftFun app =
  W.buildMiddleware internalConfig liftFun (baseAppHook app)
  where
    internalConfig = W.SpockConfigInternal maxRequestSize errorHandler logError
    errorHandler status = spockAsApp $ W.buildMiddleware W.defaultSpockConfigInternal id $ baseAppHook $ errorApp status
    errorApp status = mapM_ (\method -> hookAny method $ \_ -> errorAction' status) [minBound .. maxBound]
    errorAction' status = setStatus status >> errorAction status

baseAppHook :: forall m. MonadIO m => SpockT m () -> W.SpockAllT m m ()
baseAppHook app =
  swapMonad lifter (runSpockT app)
  where
    lifter :: forall b. ReaderT (LiftHooked () m) m b -> m b
    lifter action = runReaderT action (LiftHooked id)

-- | Specify an action that will be run when the HTTP verb 'GET' and the given route match
get :: (HasRep xs, RouteM t, Monad m) => Path xs ps -> HVectElim xs (ActionCtxT ctx m ()) -> t ctx m ()
get = hookRoute GET

-- | Specify an action that will be run when the HTTP verb 'POST' and the given route match
post :: (HasRep xs, RouteM t, Monad m) => Path xs ps -> HVectElim xs (ActionCtxT ctx m ()) -> t ctx m ()
post = hookRoute POST

-- | Specify an action that will be run when the HTTP verb 'GET'/'POST' and the given route match
getpost :: (HasRep xs, RouteM t, Monad m, Monad (t ctx m)) => Path xs ps -> HVectElim xs (ActionCtxT ctx m ()) -> t ctx m ()
getpost r a = hookRoute POST r a >> hookRoute GET r a

-- | Specify an action that will be run when the HTTP verb 'HEAD' and the given route match
head :: (HasRep xs, RouteM t, Monad m) => Path xs ps -> HVectElim xs (ActionCtxT ctx m ()) -> t ctx m ()
head = hookRoute HEAD

-- | Specify an action that will be run when the HTTP verb 'PUT' and the given route match
put :: (HasRep xs, RouteM t, Monad m) => Path xs ps -> HVectElim xs (ActionCtxT ctx m ()) -> t ctx m ()
put = hookRoute PUT

-- | Specify an action that will be run when the HTTP verb 'DELETE' and the given route match
delete :: (HasRep xs, RouteM t, Monad m) => Path xs ps -> HVectElim xs (ActionCtxT ctx m ()) -> t ctx m ()
delete = hookRoute DELETE

-- | Specify an action that will be run when the HTTP verb 'PATCH' and the given route match
patch :: (HasRep xs, RouteM t, Monad m) => Path xs ps -> HVectElim xs (ActionCtxT ctx m ()) -> t ctx m ()
patch = hookRoute PATCH

-- | Specify an action that will be run before all subroutes. It can modify the requests current context
prehook :: (RouteM t, MonadIO m) => ActionCtxT ctx m ctx' -> t ctx' m () -> t ctx m ()
prehook = withPrehook

-- | Specify an action that will be run when a standard HTTP verb and the given route match
hookRoute :: (HasRep xs, RouteM t, Monad m) => StdMethod -> Path xs ps -> HVectElim xs (ActionCtxT ctx m ()) -> t ctx m ()
hookRoute = hookRoute' . MethodStandard . W.HttpMethod

-- | Specify an action that will be run regardless of the HTTP verb
hookRouteAll :: (HasRep xs, RouteM t, Monad m) => Path xs ps -> HVectElim xs (ActionCtxT ctx m ()) -> t ctx m ()
hookRouteAll = hookRoute' MethodAny

-- | Specify an action that will be run when a custom HTTP verb and the given route match
hookRouteCustom :: (HasRep xs, RouteM t, Monad m) => T.Text -> Path xs ps -> HVectElim xs (ActionCtxT ctx m ()) -> t ctx m ()
hookRouteCustom = hookRoute' . MethodCustom

-- | Specify an action that will be run when a HTTP verb and the given route match
hookRoute' :: (HasRep xs, RouteM t, Monad m) => SpockMethod -> Path xs ps -> HVectElim xs (ActionCtxT ctx m ()) -> t ctx m ()
hookRoute' = wireRoute

-- | Specify an action that will be run when a standard HTTP verb matches but no defined route matches.
-- The full path is passed as an argument
hookAny :: (RouteM t, Monad m) => StdMethod -> ([T.Text] -> ActionCtxT ctx m ()) -> t ctx m ()
hookAny = hookAny' . MethodStandard . W.HttpMethod

-- | Specify an action that will be run regardless of the HTTP verb and no defined route matches.
-- The full path is passed as an argument
hookAnyAll :: (RouteM t, Monad m) => ([T.Text] -> ActionCtxT ctx m ()) -> t ctx m ()
hookAnyAll = hookAny' MethodAny

-- | Specify an action that will be run when a custom HTTP verb matches but no defined route matches.
-- The full path is passed as an argument
hookAnyCustom :: (RouteM t, Monad m) => T.Text -> ([T.Text] -> ActionCtxT ctx m ()) -> t ctx m ()
hookAnyCustom = hookAny' . MethodCustom

-- | Specify an action that will be run when a HTTP verb matches but no defined route matches.
-- The full path is passed as an argument
hookAny' :: (RouteM t, Monad m) => SpockMethod -> ([T.Text] -> ActionCtxT ctx m ()) -> t ctx m ()
hookAny' = wireAny

-- | Hook wai middleware into Spock
middleware :: (RouteM t, Monad m) => Wai.Middleware -> t ctx m ()
middleware = addMiddleware

-- | Combine two path components
(<//>) :: Path as 'Open -> Path bs ps -> Path (Append as bs) ps
(<//>) = (</>)

-- | Render a route applying path pieces
renderRoute :: AllHave ToHttpApiData as => Path as 'Open -> HVectElim as T.Text
renderRoute route = curryExpl (pathToRep route) (T.cons '/' . COMB.renderRoute route)
