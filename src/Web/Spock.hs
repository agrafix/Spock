{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Spock
    ( -- * Spock's core
      spock, SpockM, SpockAction
     -- * Defining routes
    , get, post, C.head, put, delete, patch, defRoute, Http.StdMethod
     -- * Handeling requests
    , request, header, cookie, body, jsonBody, files, UploadedFile (..)
    , params, param
     -- * Sending responses
    , setStatus, setHeader, redirect, setCookie, setCookie', bytes, lazyBytes
    , text, html, file, json, blaze
     -- * Adding middleware
    , middleware
      -- * Database
    , PoolOrConn (..), ConnBuilder (..), PoolCfg (..)
      -- * Accessing Database and State
    , HasSpock (runQuery, getState)
      -- * Sessions
    , SessionCfg (..)
    , readSession, writeSession, modifySession
      -- * Safe actions
    , SafeAction (..)
    , safeActionPath
      -- * Digestive Functors
    , runForm
      -- * Raw Spock
    , SpockT, ActionT, spockT
      -- * Internals for extending Spock
    , getSpockHeart, runSpockIO, WebStateM, WebState
    )
where

import Web.Spock.Core
import Web.Spock.Digestive
import Web.Spock.Monad
import Web.Spock.SafeActions
import Web.Spock.SessionManager
import Web.Spock.Types
import qualified Web.Spock.Core as C

import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Data.Pool
import qualified Network.HTTP.Types as Http

-- | Run a spock application using the warp server, a given db storageLayer and an initial state.
-- Spock works with database libraries that already implement connection pooling and
-- with those that don't come with it out of the box. For more see the 'PoolOrConn' type.
spock :: Int -> SessionCfg sess -> PoolOrConn conn -> st -> SpockM conn sess st () -> IO ()
spock port sessionCfg poolOrConn initialState defs =
    do sessionMgr <- createSessionManager sessionCfg
       connectionPool <-
           case poolOrConn of
             PCConduitPool p ->
                 return (ConduitPool p)
             PCPool p ->
                 return (DataPool p)
             PCConn cb ->
                 let pc = cb_poolConfiguration cb
                 in DataPool <$> createPool (cb_createConn cb) (cb_destroyConn cb)
                                  (pc_stripes pc) (pc_keepOpenTime pc)
                                   (pc_resPerStripe pc)
       let internalState =
               WebState
               { web_dbConn = connectionPool
               , web_sessionMgr = sessionMgr
               , web_state = initialState
               }
           runM m = runResourceT $ runReaderT (runWebStateM m) internalState

       spockT port runM $
               do middleware (sm_middleware sessionMgr)
                  hookSafeActions
                  defs

-- | Write to the current session. Note that all data is stored on the server.
-- The user only reciedes a sessionId to be identified.
writeSession :: sess -> SpockAction conn sess st ()
writeSession d =
    do mgr <- getSessMgr
       (sm_writeSession mgr) d

-- | Modify the stored session
modifySession :: (sess -> sess) -> SpockAction conn sess st ()
modifySession f =
    do mgr <- getSessMgr
       (sm_modifySession mgr) f

-- | Read the stored session
readSession :: SpockAction conn sess st sess
readSession =
    do mgr <- getSessMgr
       sm_readSession mgr
