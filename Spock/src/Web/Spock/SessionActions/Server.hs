{-# LANGUAGE RankNTypes #-}

-- | Operations requiring server-side session storage. Client-cookie and
-- disabled backends do not provide this capability.
module Web.Spock.SessionActions.Server
  ( ServerSessionManager, getServerSessionManager, requireServerSessionManager,
    clearAllSessions, mapAllSessions
  ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Web.Spock.Action (runInContext)
import Web.Spock.Internal.Monad ()
import Web.Spock.Internal.Types

-- | Inspect whether the configured, enabled backend provides server storage.
-- This does not load or allocate a visitor session.
getServerSessionManager :: SpockActionCtx ctx conn sess st
  (Maybe (ServerSessionManager (SpockActionCtx () conn sess st) sess))
getServerSessionManager = sm_serverSessions <$> getSessMgr

-- | Require server storage, throwing 'ServerSessionsRequired' if unavailable.
requireServerSessionManager :: SpockActionCtx ctx conn sess st
  (ServerSessionManager (SpockActionCtx () conn sess st) sess)
requireServerSessionManager = getServerSessionManager >>= maybe (liftIO $ throwIO ServerSessionsRequired) pure

-- | Delete every session from this server backend.
clearAllSessions :: ServerSessionManager (SpockActionCtx () conn sess st) sess -> SpockActionCtx ctx conn sess st ()
clearAllSessions manager = runInContext () $ ssm_clearAllSessions manager

-- | Atomically transform stored sessions. Transaction callbacks may be retried.
mapAllSessions :: ServerSessionManager (SpockActionCtx () conn sess st) sess ->
  (forall m. Monad m => sess -> m sess) -> SpockActionCtx ctx conn sess st ()
mapAllSessions manager f = runInContext () $ ssm_mapSessions manager f
