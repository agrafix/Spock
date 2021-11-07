{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Spock.SessionActions
  ( SessionId,
    sessionRegenerateId,
    getSessionId,
    readSession,
    writeSession,
    modifySession,
    modifySession',
    modifyReadSession,
    mapAllSessions,
    clearAllSessions,
  )
where

import Web.Spock.Action
import Web.Spock.Internal.Monad ()
import Web.Spock.Internal.SessionManager
import Web.Spock.Internal.Types

-- | Regenerate the users sessionId. This preserves all stored data. Call this prior
-- to logging in a user to prevent session fixation attacks.
sessionRegenerateId :: SpockActionCtx ctx conn sess st ()
sessionRegenerateId =
  runInContext () $
    getSessMgr >>= sm_regenerateSessionId

-- | Get the current users sessionId. Note that this ID should only be
-- shown to it's owner as otherwise sessions can be hijacked.
getSessionId :: SpockActionCtx ctx conn sess st SessionId
getSessionId =
  runInContext () $
    getSessMgr >>= sm_getSessionId

-- | Write to the current session. Note that all data is stored on the server.
-- The user only reciedes a sessionId to be identified.
writeSession :: forall sess ctx conn st. sess -> SpockActionCtx ctx conn sess st ()
writeSession d =
  do
    mgr <- getSessMgr
    runInContext () $ sm_writeSession mgr d

-- | Modify the stored session
modifySession :: (sess -> sess) -> SpockActionCtx ctx conn sess st ()
modifySession f =
  modifySession' $ \sess -> (f sess, ())

-- | Modify the stored session and return a value
modifySession' :: (sess -> (sess, a)) -> SpockActionCtx ctx conn sess st a
modifySession' f =
  do
    mgr <- getSessMgr
    runInContext () $ sm_modifySession mgr f

-- | Modify the stored session and return the new value after modification
modifyReadSession :: (sess -> sess) -> SpockActionCtx ctx conn sess st sess
modifyReadSession f =
  modifySession' $ \sess ->
    let x = f sess
     in (x, x)

-- | Read the stored session
readSession :: SpockActionCtx ctx conn sess st sess
readSession =
  runInContext () $
    do
      mgr <- getSessMgr
      sm_readSession mgr

-- | Globally delete all existing sessions. This is useful for example if you want
-- to require all users to relogin
clearAllSessions :: SpockActionCtx ctx conn sess st ()
clearAllSessions =
  do
    mgr <- getSessMgr
    runInContext () $ sm_clearAllSessions mgr

-- | Apply a transformation to all sessions. Be careful with this, as this
-- may cause many STM transaction retries.
mapAllSessions :: (forall m. Monad m => sess -> m sess) -> SpockActionCtx ctx conn sess st ()
mapAllSessions f =
  do
    mgr <- getSessMgr
    runInContext () $ sm_mapSessions mgr f
