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
      spock, SpockM
    , spockT, SpockT
     -- * Defining routes
    , Path, root, Var, var, static, (<//>)
     -- * Rendering routes
    , renderRoute
     -- * Hooking routes
    , subcomponent
    , get, post, head, put, delete, patch, hookRoute, hookAny
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
import Control.Monad.Trans
import Data.Monoid
import Data.HVect
import Network.HTTP.Types.Method
import Prelude hiding (head)
import Web.Routing.SafeRouting hiding (renderRoute)
import qualified Web.Routing.SafeRouting as SR
import qualified Data.Text as T
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

type SpockM conn sess st a = SpockT (WebStateM conn sess st) a

newtype SpockT m a
    = SpockT { runSpockT :: C.SpockAllT (SafeRouter (ActionT m) ()) m a
             } deriving (Monad, Functor, Applicative, MonadIO)

instance MonadTrans SpockT where
    lift = SpockT . lift

-- | Create a spock application using a given db storageLayer and an initial state.
-- Spock works with database libraries that already implement connection pooling and
-- with those that don't come with it out of the box. For more see the 'PoolOrConn' type.
-- Use @runSpock@ to run the app or @spockAsApp@ to create a @Wai.Application@
spock :: SessionCfg sess -> PoolOrConn conn -> st -> SpockM conn sess st () -> IO Wai.Middleware
spock sessCfg poolOrConn initSt spockAppl =
    C.spockAll SafeRouter sessCfg poolOrConn initSt (runSpockT spockAppl')
    where
      spockAppl' =
          do hookSafeActions
             spockAppl

-- | Create a raw spock application with custom underlying monad
-- Use @runSpock@ to run the app or @spockAsApp@ to create a @Wai.Application@
spockT :: (MonadIO m)
       => (forall a. m a -> IO a)
       -> SpockT m ()
       -> IO Wai.Middleware
spockT liftFun (SpockT app) =
    C.spockAllT SafeRouter liftFun app

-- | Specify an action that will be run when the HTTP verb 'GET' and the given route match
get :: MonadIO m => Path xs -> HVectElim xs (ActionT m ()) -> SpockT m ()
get = hookRoute GET

-- | Specify an action that will be run when the HTTP verb 'POST' and the given route match
post :: MonadIO m => Path xs -> HVectElim xs (ActionT m ()) -> SpockT m ()
post = hookRoute POST

-- | Specify an action that will be run when the HTTP verb 'HEAD' and the given route match
head :: MonadIO m => Path xs -> HVectElim xs (ActionT m ()) -> SpockT m ()
head = hookRoute HEAD

-- | Specify an action that will be run when the HTTP verb 'PUT' and the given route match
put :: MonadIO m => Path xs -> HVectElim xs (ActionT m ()) -> SpockT m ()
put = hookRoute PUT

-- | Specify an action that will be run when the HTTP verb 'DELETE' and the given route match
delete :: MonadIO m => Path xs -> HVectElim xs (ActionT m ()) -> SpockT m ()
delete = hookRoute DELETE

-- | Specify an action that will be run when the HTTP verb 'PATCH' and the given route match
patch :: MonadIO m => Path xs -> HVectElim xs (ActionT m ()) -> SpockT m ()
patch = hookRoute PATCH

-- | Specify an action that will be run when a HTTP verb and the given route match
hookRoute :: Monad m => StdMethod -> Path xs -> HVectElim xs (ActionT m ()) -> SpockT m ()
hookRoute m path action = SpockT $ C.hookRoute m (SafeRouterPath path) (HVectElim' action)

-- | Specify an action that will be run when a HTTP verb matches but no defined route matches.
-- The full path is passed as an argument
hookAny :: Monad m => StdMethod -> ([T.Text] -> ActionT m ()) -> SpockT m ()
hookAny m action = SpockT $ C.hookAny m action

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
subcomponent :: Monad m => Path '[] -> SpockT m () -> SpockT m ()
subcomponent p (SpockT subapp) = SpockT $ C.subcomponent (SafeRouterPath p) subapp

-- | Hook wai middleware into Spock
middleware :: Monad m => Wai.Middleware -> SpockT m ()
middleware = SpockT . C.middleware

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
    do get (static "h" </> var) run
       post (static "h" </> var) run
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
renderRoute :: HasRep as => Path as -> HVectElim as T.Text
renderRoute route = hVectCurry (T.cons '/' . SR.renderRoute route)
