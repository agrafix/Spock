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
    , readSession, writeSession, modifySession
      -- * Cookies
    , setCookie, setCookie', getCookie
      -- * Safe actions
    , SafeAction (..)
    , safeActionPath
      -- * General Routing
    , get, post, put, delete, patch, addroute, Http.StdMethod (..)
      -- * Other reexports from scotty
    , middleware, matchAny, notFound
    , request, reqHeader, body, param, params, jsonData, files
    , status, addHeader, setHeader, redirect
    , text, html, blaze, file, json, source, raw
    , raise, rescue, next
    , RoutePattern
      -- * Spock utilities
    , paramPathPiece
      -- * Internals for extending Spock
    , getSpockHeart, runSpockIO, WebStateM, WebState
    )
where

import Web.Spock.SessionManager
import Web.Spock.Monad
import Web.Spock.Types
import Web.Spock.Cookie
import Web.Spock.SafeActions

import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Data.Pool
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty.Trans
import Web.PathPieces
import qualified Network.HTTP.Types as Http
import qualified Data.Text.Lazy as TL

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
                  hookSafeActions
                  defs

-- | Output html built with blaze
blaze :: Html -> SpockAction conn sess st ()
blaze htmlVal =
    html $ renderHtml htmlVal

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

-- | Same as "param", but the target type needs to implement "PathPiece"
paramPathPiece :: PathPiece s => TL.Text -> SpockAction conn sess st s
paramPathPiece t =
    do val <- param t
       case fromPathPiece val of
         Just x ->
             return x
         Nothing ->
             raise $ stringError $ "Cannot convert param: " ++ TL.unpack t ++ " to path piece!"
