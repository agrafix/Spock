{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
Since version 0.7 Spock features a new routing system that enables more type-safe code while still being relatively simple and lightweight.
You should consider using that (see "Web.Spock.Safe") instead of this module. This module is not yet deprecated, but this may happen anytime soon.
-}
module Web.Spock.Simple
    ( -- * Spock's route definition monad
      spock, SpockM
    , spockT, spockLimT, SpockT
     -- * Defining routes
    , SpockRoute, (<//>)
     -- * Hooking routes
    , subcomponent
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
import Control.Monad.Trans
import Data.Monoid
import Data.String
import Data.Word
import Network.HTTP.Types.Method
import Prelude hiding (head)
import Web.Routing.TextRouting
import qualified Data.Text as T
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

type SpockM conn sess st a = SpockT (WebStateM conn sess st) a

newtype SpockT m a
    = SpockT { runSpockT :: C.SpockAllT (TextRouter (ActionT m) ()) m a
             } deriving (Monad, Functor, Applicative, MonadIO)

instance MonadTrans SpockT where
    lift = SpockT . lift

newtype SpockRoute
    = SpockRoute { _unSpockRoute :: T.Text }
    deriving (Eq, Ord, Show, Read)

instance IsString SpockRoute where
    fromString str = SpockRoute $ combineRoute (T.pack str) ""

-- | Create a spock application using a given db storageLayer and an initial state.
-- Spock works with database libraries that already implement connection pooling and
-- with those that don't come with it out of the box. For more see the 'PoolOrConn' type.
-- Use @runSpock@ to run the app or @spockAsApp@ to create a @Wai.Application@
spock :: SpockCfg conn sess st -> SpockM conn sess st () -> IO Wai.Middleware
spock cfg spockAppl =
    C.spockAll TextRouter cfg (runSpockT spockAppl')
    where
      spockAppl' =
          do hookSafeActions
             spockAppl

-- | Create a raw spock application with custom underlying monad
-- Use @runSpock@ to run the app or @spockAsApp@ to create a @Wai.Application@.
spockT :: (MonadIO m)
       => (forall a. m a -> IO a)
       -> SpockT m ()
       -> IO Wai.Middleware
spockT = spockLimT Nothing

-- | Like @spockT@, but the first argument is request size limit in bytes. Set to 'Nothing' to disable.
spockLimT :: (MonadIO m)
       => Maybe Word64
       -> (forall a. m a -> IO a)
       -> SpockT m ()
       -> IO Wai.Middleware
spockLimT mSizeLimit liftFun (SpockT app) =
    C.spockAllT mSizeLimit TextRouter liftFun app

-- | Combine two route components safely
--
-- >>> "/foo" <//> "/bar"
-- "/foo/bar"
--
-- >>> "foo" <//> "bar"
-- "/foo/bar"
--
-- >>> "foo <//> "/bar"
-- "/foo/bar"
(<//>) :: SpockRoute -> SpockRoute -> SpockRoute
(SpockRoute t) <//> (SpockRoute t') = SpockRoute $ combineRoute t t'

-- | Specify an action that will be run when the HTTP verb 'GET' and the given route match
get :: MonadIO m => SpockRoute -> ActionT m () -> SpockT m ()
get = hookRoute GET

-- | Specify an action that will be run when the HTTP verb 'POST' and the given route match
post :: MonadIO m => SpockRoute -> ActionT m () -> SpockT m ()
post = hookRoute POST

-- | Specify an action that will be run when the HTTP verb 'GET'/'POST' and the given route match
getpost :: MonadIO m => SpockRoute -> ActionT m () -> SpockT m ()
getpost r a = hookRoute POST r a >> hookRoute GET r a

-- | Specify an action that will be run when the HTTP verb 'HEAD' and the given route match
head :: MonadIO m => SpockRoute -> ActionT m () -> SpockT m ()
head = hookRoute HEAD

-- | Specify an action that will be run when the HTTP verb 'PUT' and the given route match
put :: MonadIO m => SpockRoute -> ActionT m () -> SpockT m ()
put = hookRoute PUT

-- | Specify an action that will be run when the HTTP verb 'DELETE' and the given route match
delete :: MonadIO m => SpockRoute -> ActionT m () -> SpockT m ()
delete = hookRoute DELETE

-- | Specify an action that will be run when the HTTP verb 'PATCH' and the given route match
patch :: MonadIO m => SpockRoute -> ActionT m () -> SpockT m ()
patch = hookRoute PATCH

-- | Specify an action that will be run when a HTTP verb and the given route match
hookRoute :: Monad m => StdMethod -> SpockRoute -> ActionT m () -> SpockT m ()
hookRoute m (SpockRoute path) action = SpockT $ C.hookRoute m (TextRouterPath path) (TAction action)

-- | Specify an action that will be run when a HTTP verb matches but no defined route matches.
-- The full path is passed as an argument
hookAny :: Monad m => StdMethod -> ([T.Text] -> ActionT m ()) -> SpockT m ()
hookAny m action = SpockT $ C.hookAny m action

-- | Define a subcomponent. Usage example:
--
-- > subcomponent "site" $
-- >   do get "home" homeHandler
-- >      get ("misc" <//> ":param") $ -- ...
-- > subcomponent "/admin" $
-- >   get "home" adminHomeHandler
--
-- The request \/site\/home will be routed to homeHandler and the
-- request \/admin\/home will be routed to adminHomeHandler
subcomponent :: Monad m => SpockRoute -> SpockT m () -> SpockT m ()
subcomponent (SpockRoute p) (SpockT subapp) = SpockT $ C.subcomponent (TextRouterPath p) subapp

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
-- > get ("user-details" <//> ":userId") $
-- >   do userId <- param' "userId"
-- >      deleteUrl <- safeActionPath (DeleteUser userId)
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
    getpost ("h" <//> ":spock-csurf-protection") run
    where
      run =
          do Just h <- param "spock-csurf-protection"
             mgr <- getSessMgr
             mAction <- sm_lookupSafeAction mgr h
             case mAction of
               Nothing ->
                   do setStatus Http.status404
                      text "File not found"
               Just p@(PackedSafeAction action) ->
                   do runSafeAction action
                      sm_removeSafeAction mgr p
