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
    sessionDestroy,
    getSessionId,
    readSession,
    writeSession,
    modifySession,
    modifySession',
    modifyReadSession,
  )
where

import Web.Spock.Action
import Web.Spock.Internal.Monad ()
import Web.Spock.Internal.SessionManager
import Web.Spock.Internal.Types

-- | Regenerate the users sessionId. This preserves all stored data. Call this prior
-- to logging in a user to prevent session fixation attacks.
-- Server backends revoke the old ID; a client backend issues new ID/CSRF values
-- but cannot revoke copies of previously issued cookies.
sessionRegenerateId :: SpockActionCtx ctx conn sess st ()
sessionRegenerateId =
  runInContext () $
    getSessMgr >>= sm_regenerateSessionId

-- | Expire this browser's cookie and discard the current request's session.
-- Server backends also revoke the stored session. Previously issued stateless
-- cookies remain replayable until expiry or key removal. No replacement is
-- created until another session action is used. Use a CSRF-protected logout.
sessionDestroy :: SpockActionCtx ctx conn sess st ()
sessionDestroy = runInContext () $ getSessMgr >>= sm_destroySession

-- | Get the current users sessionId. Note that this ID should only be
-- shown to it's owner as otherwise sessions can be hijacked.
getSessionId :: SpockActionCtx ctx conn sess st SessionId
getSessionId =
  runInContext () $
    getSessMgr >>= sm_getSessionId

-- | Write to the current session using the configured server or cookie backend.
writeSession :: forall sess ctx conn st. sess -> SpockActionCtx ctx conn sess st ()
writeSession d =
  do
    mgr <- getSessMgr
    runInContext () $ sm_writeSession mgr d

-- | Modify the current session. Server backends perform this atomically in the
-- store. Client backends modify this request's copy; simultaneous requests can
-- overwrite one another when the browser accepts their response cookies.
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
