{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Spock.Shared
    (-- * Helpers for running Spock
      runSpock, spockAsApp
     -- * Action types
    , SpockAction, SpockActionCtx, ActionT, W.ActionCtxT
     -- * Handling requests
    , request, header, rawHeader, cookie, reqMethod
    , preferredFormat, ClientPreferredFormat(..)
    , body, jsonBody, jsonBody'
    , files, UploadedFile (..)
    , params, param, param'
     -- * Working with context
    , getContext, runInContext
     -- * Sending responses
    , setStatus, setHeader, redirect, jumpNext, CookieSettings(..), defaultCookieSettings, CookieEOL(..), setCookie, deleteCookie, bytes, lazyBytes
    , text, html, file, json, stream, response
      -- * Middleware helpers
    , middlewarePass, modifyVault, queryVault
      -- * Configuration
    , SpockCfg (..), defaultSpockCfg
      -- * Database
    , PoolOrConn (..), ConnBuilder (..), PoolCfg (..)
      -- * Accessing Database and State
    , HasSpock (runQuery, getState), SpockConn, SpockState, SpockSession
      -- * Basic HTTP-Auth
    , requireBasicAuth
     -- * Sessions
    , defaultSessionCfg, SessionCfg (..)
    , defaultSessionHooks, SessionHooks (..)
    , SessionPersistCfg(..), readShowSessionPersist
    , SessionId
    , getSessionId, readSession, writeSession
    , modifySession, modifySession', modifyReadSession, mapAllSessions, clearAllSessions
     -- * Internals for extending Spock
    , getSpockHeart, runSpockIO, WebStateM, WebState
    )
where

import Web.Spock.Internal.Monad
import Web.Spock.Internal.SessionManager
import Web.Spock.Internal.Types
import Web.Spock.Internal.CoreAction
import Control.Monad
import Control.Concurrent.STM (STM)
import System.Directory
import qualified Web.Spock.Internal.Wire as W
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

-- | Run a Spock application. Basically just a wrapper aroung @Warp.run@.
runSpock :: Warp.Port -> IO Wai.Middleware -> IO ()
runSpock port mw =
    do putStrLn ("Spock is running on port " ++ show port)
       app <- spockAsApp mw
       Warp.run port app

-- | Convert a middleware to an application. All failing requests will
-- result in a 404 page
spockAsApp :: IO Wai.Middleware -> IO Wai.Application
spockAsApp = liftM W.middlewareToApp

-- | Get the current users sessionId. Note that this ID should only be
-- shown to it's owner as otherwise sessions can be hijacked.
getSessionId :: SpockActionCtx ctx conn sess st SessionId
getSessionId =
    getSessMgr >>= sm_getSessionId

-- | Write to the current session. Note that all data is stored on the server.
-- The user only reciedes a sessionId to be identified.
writeSession :: sess -> SpockActionCtx ctx conn sess st ()
writeSession d =
    do mgr <- getSessMgr
       sm_writeSession mgr d

-- | Modify the stored session
modifySession :: (sess -> sess) -> SpockActionCtx ctx conn sess st ()
modifySession f =
    modifySession' $ \sess -> (f sess, ())

-- | Modify the stored session and return a value
modifySession' :: (sess -> (sess, a)) -> SpockActionCtx ctx conn sess st a
modifySession' f =
    do mgr <- getSessMgr
       sm_modifySession mgr f

-- | Modify the stored session and return the new value after modification
modifyReadSession :: (sess -> sess) -> SpockActionCtx ctx conn sess st sess
modifyReadSession f =
    modifySession' $ \sess ->
        let x = f sess
        in (x, x)

-- | Read the stored session
readSession :: SpockActionCtx ctx conn sess st sess
readSession =
    do mgr <- getSessMgr
       sm_readSession mgr

-- | Globally delete all existing sessions. This is useful for example if you want
-- to require all users to relogin
clearAllSessions :: SpockActionCtx ctx conn sess st ()
clearAllSessions =
    do mgr <- getSessMgr
       sm_clearAllSessions mgr

-- | Apply a transformation to all sessions. Be careful with this, as this
-- may cause many STM transaction retries.
mapAllSessions :: (sess -> STM sess) -> SpockActionCtx ctx conn sess st ()
mapAllSessions f =
    do mgr <- getSessMgr
       sm_mapSessions mgr f

-- | Simple session persisting configuration. DO NOT USE IN PRODUCTION
readShowSessionPersist :: (Read a, Show a) => FilePath -> SessionPersistCfg a
readShowSessionPersist fp =
    SessionPersistCfg
    { spc_load =
         do isThere <- doesFileExist fp
            if isThere
            then do str <- readFile fp
                    return (read str)
            else return []
    , spc_store = writeFile fp . show
    }
