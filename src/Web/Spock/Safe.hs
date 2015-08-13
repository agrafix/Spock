{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
This module implements the type safe routing aproach. It should be used by all new Spock powered applications. To learn more about
the routing, read the corresponding blog post available at <http://www.spock.li/2015/04/19/type-safe_routing.html>
-}
module Web.Spock.Safe
    ( -- * Spock's route definition monad
      spock, SpockM, SpockCtxM
    , spockT, spockLimT, SpockT, SpockCtxT
     -- * Defining routes
    , Path, root, Var, var, static, (<//>)
     -- * Rendering routes
    , renderRoute
     -- * Hooking routes
    , subcomponent, prehook
    , get, post, getpost, head, put, delete, patch, hookRoute, hookAny
    , Http.StdMethod (..)
      -- * Adding Wai.Middleware
    , middleware
      -- * Safe actions
    , SafeAction (..)
    , safeActionPath
    , module Web.Spock.Shared
    )
where


import Web.Spock.Shared
import Web.Spock.Internal.Types
import qualified Web.Spock.Internal.Core as C

import Control.Applicative
import Control.Monad.Reader
import Data.HVect hiding (head)
import Data.Monoid
import Data.Word
import Network.HTTP.Types.Method
import Prelude hiding (head, uncurry, curry)
import Web.Routing.AbstractRouter (swapMonad)
import Web.Routing.SafeRouting hiding (renderRoute)
import qualified Data.Text as T
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Web.Routing.SafeRouting as SR

type SpockM conn sess st = SpockCtxM () conn sess st
type SpockCtxM ctx conn sess st = SpockCtxT ctx (WebStateM conn sess st)

type SpockT = SpockCtxT ()

newtype LiftHooked ctx m =
    LiftHooked { unLiftHooked :: forall a. ActionCtxT ctx m a -> ActionCtxT () m a }

injectHook :: LiftHooked ctx m -> (forall a. ActionCtxT ctx' m a -> ActionCtxT ctx m a) -> LiftHooked ctx' m
injectHook (LiftHooked baseHook) nextHook =
    LiftHooked $ baseHook . nextHook

newtype SpockCtxT ctx m a
    = SpockCtxT
    { runSpockT :: C.SpockAllT (SafeRouter (ActionT m) ()) (ReaderT (LiftHooked ctx m) m) a
    } deriving (Monad, Functor, Applicative, MonadIO)

instance MonadTrans (SpockCtxT ctx) where
    lift = SpockCtxT . lift . lift

-- | Create a spock application using a given db storageLayer and an initial state.
-- Spock works with database libraries that already implement connection pooling and
-- with those that don't come with it out of the box. For more see the 'PoolOrConn' type.
-- Use @runSpock@ to run the app or @spockAsApp@ to create a @Wai.Application@
spock :: SpockCfg conn sess st -> SpockM conn sess st () -> IO Wai.Middleware
spock spockCfg spockAppl =
    C.spockAll SafeRouter spockCfg (baseAppHook spockAppl')
    where
      spockAppl' =
          do hookSafeActions
             spockAppl

-- | Create a raw spock application with custom underlying monad
-- Use @runSpock@ to run the app or @spockAsApp@ to create a @Wai.Application@
-- The first argument is request size limit in bytes. Set to 'Nothing' to disable.
spockT :: (MonadIO m)
       => (forall a. m a -> IO a)
       -> SpockT m ()
       -> IO Wai.Middleware
spockT = spockLimT Nothing

-- | Like @spockT@, but first argument is request size limit in bytes. Set to 'Nothing' to disable.
spockLimT :: forall m .MonadIO m
       => Maybe Word64
       -> (forall a. m a -> IO a)
       -> SpockT m ()
       -> IO Wai.Middleware
spockLimT mSizeLimit liftFun app =
    C.spockAllT mSizeLimit SafeRouter liftFun (baseAppHook app)

baseAppHook :: forall m. MonadIO m => SpockT m () -> C.SpockAllT (SafeRouter (ActionT m) ()) m ()
baseAppHook app =
    swapMonad lifter (runSpockT app)
    where
      lifter :: forall b. ReaderT (LiftHooked () m) m b -> m b
      lifter action = runReaderT action (LiftHooked id)

-- | Specify an action that will be run when the HTTP verb 'GET' and the given route match
get :: (HasRep xs, MonadIO m) => Path xs -> HVectElim xs (ActionCtxT ctx m ()) -> SpockCtxT ctx m ()
get = hookRoute GET

-- | Specify an action that will be run when the HTTP verb 'POST' and the given route match
post :: (HasRep xs, MonadIO m) => Path xs -> HVectElim xs (ActionCtxT ctx m ()) -> SpockCtxT ctx m ()
post = hookRoute POST

-- | Specify an action that will be run when the HTTP verb 'GET'/'POST' and the given route match
getpost :: (HasRep xs, MonadIO m) => Path xs -> HVectElim xs (ActionCtxT ctx m ()) -> SpockCtxT ctx m ()
getpost r a = hookRoute POST r a >> hookRoute GET r a

-- | Specify an action that will be run when the HTTP verb 'HEAD' and the given route match
head :: (HasRep xs, MonadIO m) => Path xs -> HVectElim xs (ActionCtxT ctx m ()) -> SpockCtxT ctx m ()
head = hookRoute HEAD

-- | Specify an action that will be run when the HTTP verb 'PUT' and the given route match
put :: (HasRep xs, MonadIO m) => Path xs -> HVectElim xs (ActionCtxT ctx m ()) -> SpockCtxT ctx m ()
put = hookRoute PUT

-- | Specify an action that will be run when the HTTP verb 'DELETE' and the given route match
delete :: (HasRep xs, MonadIO m) => Path xs -> HVectElim xs (ActionCtxT ctx m ()) -> SpockCtxT ctx m ()
delete = hookRoute DELETE

-- | Specify an action that will be run when the HTTP verb 'PATCH' and the given route match
patch :: (HasRep xs, MonadIO m) => Path xs -> HVectElim xs (ActionCtxT ctx m ()) -> SpockCtxT ctx m ()
patch = hookRoute PATCH

-- | Specify an action that will be run before all subroutes. It can modify the requests current context
prehook :: forall m ctx ctx'. MonadIO m => ActionCtxT ctx m ctx' -> SpockCtxT ctx' m () -> SpockCtxT ctx m ()
prehook hook (SpockCtxT hookBody) =
    SpockCtxT $
    do prevHook <- lift ask
       let newHook :: ActionCtxT ctx' m a -> ActionCtxT ctx m a
           newHook act =
               do newCtx <- hook
                  runInContext newCtx act
           hookLift :: forall a. ReaderT (LiftHooked ctx' m) m a -> ReaderT (LiftHooked ctx m) m a
           hookLift a =
               lift $ runReaderT a (injectHook prevHook newHook)
       swapMonad hookLift hookBody

-- | Specify an action that will be run when a HTTP verb and the given route match
hookRoute :: forall xs ctx m. (HasRep xs, Monad m) => StdMethod -> Path xs -> HVectElim xs (ActionCtxT ctx m ()) -> SpockCtxT ctx m ()
hookRoute m path action =
    SpockCtxT $
    do hookLift <- lift $ asks unLiftHooked
       let actionPacker :: HVectElim xs (ActionCtxT ctx m ()) -> HVect xs -> ActionCtxT () m ()
           actionPacker act captures = hookLift (uncurry act captures)
       C.hookRoute m (SafeRouterPath path) (HVectElim' $ curry $ actionPacker action)

-- | Specify an action that will be run when a HTTP verb matches but no defined route matches.
-- The full path is passed as an argument
hookAny :: Monad m => StdMethod -> ([T.Text] -> ActionCtxT ctx m ()) -> SpockCtxT ctx m ()
hookAny m action =
    SpockCtxT $
    do hookLift <- lift $ asks unLiftHooked
       C.hookAny m (hookLift . action)

-- | Define a subcomponent. Usage example:
--
-- > subcomponent "site" $
-- >   do get "home" homeHandler
-- >      get ("misc" <//> var) $ -- ...
-- > subcomponent "admin" $
-- >   do get "home" adminHomeHandler
--
-- The request \/site\/home will be routed to homeHandler and the
-- request \/admin\/home will be routed to adminHomeHandler
subcomponent :: Monad m => Path '[] -> SpockCtxT ctx m () -> SpockCtxT ctx m ()
subcomponent p (SpockCtxT subapp) = SpockCtxT $ C.subcomponent (SafeRouterPath p) subapp

-- | Hook wai middleware into Spock
middleware :: Monad m => Wai.Middleware -> SpockCtxT ctx m ()
middleware = SpockCtxT . C.middleware

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
                  , SpockConn (SpockAction conn sess st) ~ conn
                  , SpockSession (SpockAction conn sess st) ~ sess
                  , SpockState (SpockAction conn sess st) ~ st)
               => a
               -> SpockAction conn sess st T.Text
safeActionPath safeAction =
    do mgr <- getSessMgr
       hash <- sm_addSafeAction mgr (PackedSafeAction safeAction)
       return $ "/h/" <> hash

hookSafeActions :: forall conn sess st.
                   ( HasSpock (SpockAction conn sess st)
                   , SpockConn (SpockAction conn sess st) ~ conn
                   , SpockSession (SpockAction conn sess st) ~ sess
                   , SpockState (SpockAction conn sess st) ~ st)
                => SpockM conn sess st ()
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

-- | Combine two path components
(<//>) :: Path as -> Path bs -> Path (Append as bs)
(<//>) = (</>)

-- | Render a route applying path pieces
renderRoute :: Path as -> HVectElim as T.Text
renderRoute route = curryExpl (pathToRep route) (T.cons '/' . SR.renderRoute route)
