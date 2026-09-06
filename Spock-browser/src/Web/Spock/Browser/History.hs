{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Scoped History API navigation. Mount one router per window, and call
-- 'unmountRouter' when its owner is removed. Same-origin links inside the scope
-- are intercepted; external, download, modified and same-page anchor clicks
-- retain their browser behavior. popstate and hashchange are deduplicated.
module Web.Spock.Browser.History
  ( BrowserConfig (..), MountedRouter, HistoryMode (..), mountRouter,
    navigate, currentLocation, unmountRouter
  ) where

import Control.Concurrent.MVar
import Control.Exception (mask, mask_, onException)
import Control.Monad (unless, when)
import Data.IORef
import qualified Data.Text as T
import GHC.JS.Foreign.Callback
import GHC.JS.Prim
import Web.Spock.Browser

-- | Explicit URL scope and application handlers. Scope is '/' or an absolute
-- path prefix such as '/app'; prefix matching respects segment boundaries.
data BrowserConfig = BrowserConfig
  { bc_scope :: T.Text,
    bc_notFound :: Location -> IO (),
    bc_navigationError :: NavigationError -> IO ()
  }

-- | Owned listeners and callback. 'unmountRouter' is idempotent. Queued events
-- are ignored after unmount; a handler already running is allowed to finish.
data MountedRouter = MountedRouter JSVal (Callback (JSVal -> IO ())) (IORef Bool)

-- | Add a history entry or replace the current entry. Navigating to the current
-- URL rerenders without adding a duplicate history entry.
data HistoryMode = Push | Replace deriving (Eq, Show)

-- | Attach listeners and synchronously render the current in-scope URL.
-- Navigation handlers run serially, in event order. Mounting twice returns
-- AlreadyMounted; if the initial handler throws, listeners are cleaned up.
mountRouter :: BrowserConfig -> Router -> IO (Either NavigationError MountedRouter)
mountRouter cfg router = case parseLocation (bc_scope cfg) of
  Left err -> pure $ Left err
  Right scope | not (T.null $ locationQuery scope) || not (T.null $ locationFragment scope) -> pure $ Left InvalidLocation
  Right _ -> mask $ \restore -> do
    active <- newIORef True
    lock <- newMVar ()
    let run value = withMVar lock $ \() -> do
          alive <- readIORef active
          when alive $ do
            code <- fromJSInt <$> getProp value "error"
            if code /= 0 then bc_navigationError cfg (fromCode code) else do
              url <- T.pack . fromJSString <$> getProp value "url"
              case parseLocation url of
                Left err -> bc_navigationError cfg err
                Right location -> dispatch router location >>= \matched -> unless matched $ bc_notFound cfg location
    callback <- asyncCallback1 run
    handle <- js_mount (toJSString $ T.unpack $ bc_scope cfg) callback `onException` releaseCallback callback
    code <- fromJSInt <$> getProp handle "error"
    if code /= 0 then releaseCallback callback >> pure (Left $ fromCode code) else do
      let mounted = MountedRouter handle callback active
      restore (js_initial handle >>= \value -> do
        present <- fromJSInt <$> getProp value "present"
        when (present /= 0) $ run value) `onException` unmountRouter mounted
      pure $ Right mounted

-- | Navigate to a validated local URL within this router's scope. Rendering is
-- queued through the same callback as link clicks, so handlers may navigate
-- again without deadlocking. History API failures leave rendering unchanged.
navigate :: MountedRouter -> HistoryMode -> T.Text -> IO (Either NavigationError ())
navigate (MountedRouter handle _ active) mode url = do
  alive <- readIORef active
  if not alive then pure $ Left RouterStopped else case parseLocation url of
    Left err -> pure $ Left err
    Right _ -> do
      code <- js_navigate handle (mode == Replace) (toJSString $ T.unpack url)
      pure $ if code == 0 then Right () else Left (fromCode code)

-- | Read the current browser path, query and fragment without exposing origin
-- or credentials. Query strings remain encoded for application-specific parsing.
currentLocation :: IO (Either NavigationError Location)
currentLocation = parseLocation . T.pack . fromJSString <$> js_location

-- | Remove all listeners and release the Haskell callback exactly once.
unmountRouter :: MountedRouter -> IO ()
unmountRouter (MountedRouter handle callback active) = mask_ $ do
  wasActive <- atomicModifyIORef' active (\old -> (False, old))
  when wasActive $ js_unmount handle >> releaseCallback callback

fromCode :: Int -> NavigationError
fromCode 1 = InvalidLocation
fromCode 2 = OutsideScope
fromCode 4 = AlreadyMounted
fromCode 5 = RouterStopped
fromCode _ = HistoryUnavailable

foreign import javascript unsafe "h$spock_history_mount"
  js_mount :: JSVal -> Callback (JSVal -> IO ()) -> IO JSVal
foreign import javascript unsafe "h$spock_history_initial"
  js_initial :: JSVal -> IO JSVal
foreign import javascript unsafe "h$spock_history_navigate"
  js_navigate :: JSVal -> Bool -> JSVal -> IO Int
foreign import javascript unsafe "h$spock_history_unmount"
  js_unmount :: JSVal -> IO ()
foreign import javascript unsafe "(() => location.pathname + location.search + location.hash)"
  js_location :: IO JSVal
