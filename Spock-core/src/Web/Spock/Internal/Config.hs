{-# LANGUAGE RankNTypes #-}
module Web.Spock.Internal.Config where

import qualified Web.Spock.Internal.Wire as W

import Data.Word
import Network.HTTP.Types.Status
import System.IO
import Web.Spock.Internal.CoreAction
import qualified Data.Text as T
import qualified Data.Text.IO as T


data SpockConfig
    = SpockConfig
    { sc_maxRequestSize :: Maybe Word64
      -- ^ Maximum request size in bytes
    , sc_errorHandler :: Status -> W.ActionCtxT () IO ()
      -- ^ Error handler. Given status is set in response by default, but you
      -- can always override it with `setStatus`
    , sc_logError :: T.Text -> IO ()
      -- ^ Function that should be called to log errors.
    }

-- | Default Spock configuration. No restriction on maximum request size; error
-- handler simply prints status message as plain text and all errors are logged
-- to stderr.
defaultSpockConfig :: SpockConfig
defaultSpockConfig = SpockConfig Nothing defaultHandler (T.hPutStrLn stderr)
  where
    defaultHandler = bytes . statusMessage
