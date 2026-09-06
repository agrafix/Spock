{-# LANGUAGE RankNTypes #-}

module Web.Spock.Internal.Config where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Word
import Network.HTTP.Types.Status
import System.IO
import Web.Spock.Internal.CoreAction
import Web.Spock.Logging
import Web.Routing.SafeRouting (SlashPolicy (..))
import qualified Web.Spock.Internal.Wire as W

data SpockConfig = SpockConfig
  { -- | Maximum request body size in bytes, checked as an action reads the
    -- body (including JSON, form parameters, and uploads). An unused body is
    -- not read or rejected based on its declared length. Exceeding the limit
    -- invokes 'sc_errorHandler' with status 413.
    sc_maxRequestSize :: Maybe Word64,
    -- | Error handler. Given status is set in response by default, but you
    -- can always override it with `setStatus`
    sc_errorHandler :: Status -> W.ActionCtxT () IO (),
    -- | Function that should be called to log errors.
    sc_logError :: T.Text -> IO (),
    -- | Optional request IDs and structured handler/access/error events.
    sc_logging :: Maybe LoggingConfig,
    -- | Empty-segment matching. 'IgnoreSlashes' retains historical behavior.
    -- 'StrictSlashes' distinguishes @/foo@ and @/foo/@. 'RedirectTrailingSlashes'
    -- uses strict matching, then sends 308 if changing only the final slash
    -- finds a route for the same method. A matching wildcard or fallback wins.
    sc_slashPolicy :: SlashPolicy
  }

-- | Default Spock configuration. No restriction on maximum request size; error
-- handler simply prints status message as plain text and all errors are logged
-- to stderr.
defaultSpockConfig :: SpockConfig
defaultSpockConfig = SpockConfig Nothing defaultHandler (T.hPutStrLn stderr) Nothing IgnoreSlashes
  where
    defaultHandler = bytes . statusMessage
