{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Read requests and produce responses inside route handlers.
-- This module is part of @Spock-core@ and is also reexported by @Web.Spock@.
-- Its actions work in both core and full Spock applications.
--
-- = Choose a request reader
--
-- * 'param' returns an optional typed query/form value; 'param'' returns
--   status 400 when a required value is missing or cannot be parsed.
-- * 'paramsGet' and 'paramsPost' distinguish query and form parameters.
-- * 'jsonBody' returns optional parsed JSON; 'jsonBody'' rejects invalid JSON
--   with status 400. 'body' gives the cached raw bytes.
-- * 'header' decodes a header as text; 'rawHeader' preserves its bytes.
-- * 'filesMulti' groups uploaded files by field name. Process them during the
--   action because their temporary files are removed after the request.
--
-- = Send a response
--
-- Set status and headers with 'setStatus' and 'setHeader', then call 'text',
-- 'html', 'json', 'file', or another response helper. Sending a response
-- finishes the action. 'lazyBytes' requires you to choose the content type.
--
-- For HTML templates, see the
-- <https://www.spock.li/tutorials/rendering Blaze and Lucid rendering tutorial>.
module Web.Spock.Action
  ( -- * Action types
    ActionT,
    W.ActionCtxT,

    -- * Handling requests
    request,
    header,
    rawHeader,
    cookies,
    getRequestId,
    logMessage,
    module Web.Spock.Logging,
    cookie,
    reqMethod,
    preferredFormat,
    ClientPreferredFormat (..),
    body,
    jsonBody,
    jsonBody',
    files,
    filesMulti,
    UploadedFile (..),
    params,
    paramsGet,
    paramsPost,
    param,
    param',

    -- * Working with context
    getContext,
    runInContext,

    -- * Sending responses
    setStatus,
    setHeader,
    redirect,
    jumpNext,
    CookieSettings (..),
    SameSite (..),
    defaultCookieSettings,
    CookieEOL (..),
    setCookie,
    deleteCookie,
    bytes,
    lazyBytes,
    setRawMultiHeader,
    MultiHeader (..),
    text,
    html,
    file,
    json,
    stream,
    response,
    respondApp,
    respondMiddleware,

    -- * Middleware helpers
    middlewarePass,
    modifyVault,
    queryVault,

    -- * Basic HTTP-Auth
    requireBasicAuth,
    withBasicAuthData,
  )
where

import Web.Spock.Internal.CoreAction
import Web.Spock.Internal.Cookies (SameSite (..))
import Web.Spock.Logging
import qualified Web.Spock.Internal.Wire as W
