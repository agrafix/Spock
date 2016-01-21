{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Spock.Action
    ( -- * Action types
      SpockAction, SpockActionCtx, ActionT, W.ActionCtxT
     -- * Handling requests
    , request, header, rawHeader, cookies, cookie, reqMethod
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
      -- * Accessing Database and State
    , HasSpock (runQuery, getState), SpockConn, SpockState, SpockSession
      -- * Basic HTTP-Auth
    , requireBasicAuth, withBasicAuthData
      -- * Sessions
    , SessionId
    , sessionRegenerateId, getSessionId, readSession, writeSession
    , modifySession, modifySession', modifyReadSession, mapAllSessions, clearAllSessions
     -- * Internals for extending Spock
    , getSpockHeart, runSpockIO, WebStateM, WebState
    )
where

import Web.Spock.Internal.Monad
import Web.Spock.Internal.SessionManager
import Web.Spock.Internal.Types
import Web.Spock.Internal.CoreAction
import qualified Web.Spock.Internal.Wire as W

-- | Regenerate the users sessionId. This preserves all stored data. Call this prior
-- to logging in a user to prevent session fixation attacks.
sessionRegenerateId :: SpockActionCtx ctx conn sess st ()
sessionRegenerateId =
    getSessMgr >>= sm_regenerateSessionId

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
mapAllSessions :: (forall m. Monad m => sess -> m sess) -> SpockActionCtx ctx conn sess st ()
mapAllSessions f =
    do mgr <- getSessMgr
       sm_mapSessions mgr f
