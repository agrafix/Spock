{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Spock
    ( -- * Spock's core
      spock, SpockM, SpockAction
      -- * Database
    , PoolOrConn (..), ConnBuilder (..), PoolCfg (..)
      -- * Accessing Database and State
    , HasSpock (runQuery, getState)
    -- * Sessions
    , SessionCfg (..)
    , readSession, writeSession
      -- * Cookies
    , setCookie, setCookie', getCookie
      -- * General Routing
    , get, post, put, delete, patch, addroute, Http.StdMethod (..)
      -- * Other reexports from scotty
    , middleware, matchAny, notFound
    , request, reqHeader, body, param, params, jsonData, files
    , status, addHeader, setHeader, redirect
    , text, html, file, json, source, raw
    , raise, rescue, next
      -- * Internals for extending Spock
    , getSpockHeart, runSpockIO, WebStateM, WebState
    )
where

import Web.Spock.SessionManager
import Web.Spock.Monad
import Web.Spock.Types
import Web.Spock.Cookie

import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Data.Pool
import Web.Scotty.Trans
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
           runActionToIO = runM

       scottyT port runM runActionToIO $
               do middleware (sm_middleware sessionMgr)
                  defs

-- | Write to the current session. Note that all data is stored on the server.
-- The user only reciedes a sessionId to be identified.
writeSession :: sess -> SpockAction conn sess st ()
writeSession d =
    do mgr <- getSessMgr
       (sm_writeSession mgr) d

-- | Read the stored session
readSession :: SpockAction conn sess st sess
readSession =
    do mgr <- getSessMgr
       sm_readSession mgr
