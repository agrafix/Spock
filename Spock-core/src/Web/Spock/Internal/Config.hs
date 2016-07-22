{-# LANGUAGE RankNTypes #-}
module Web.Spock.Internal.Config where

import Data.Word
import Network.HTTP.Types.Status
import Web.Spock.Internal.CoreAction
import qualified Web.Spock.Internal.Wire as W


data SpockConfig
    = SpockConfig
    { sc_maxRequestSize :: Maybe Word64
      -- ^ Maximum request size in bytes
    , sc_errorHandler :: Status -> W.ActionCtxT () IO ()
      -- ^ Error handler. Given status is set in response by default, but you
      -- can always override it with `setStatus`
    }

-- | Default Spock configuration. No restriction on maximum request size; error
-- handler simply prints status message as plain text.
defaultSpockConfig :: SpockConfig
defaultSpockConfig = SpockConfig Nothing defaultHandler
  where
    defaultHandler = bytes . statusMessage
