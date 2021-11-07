{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Spock.Action
  ( -- * Action types
    ActionT,
    W.ActionCtxT,

    -- * Handling requests
    request,
    header,
    rawHeader,
    cookies,
    cookie,
    reqMethod,
    preferredFormat,
    ClientPreferredFormat (..),
    body,
    jsonBody,
    jsonBody',
    files,
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
import qualified Web.Spock.Internal.Wire as W
