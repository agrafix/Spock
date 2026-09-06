{-# LANGUAGE OverloadedStrings #-}

module Main where

import ApiDefinitions
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Web.Spock.Api.Server
import Web.Spock.Core

main :: IO ()
main = do
  document <- either (fail . show) pure apiDocument
  args <- getArgs
  if args == ["--openapi"]
    then LBS.putStrLn $ encode document
    else runSpock 8082 $ spockT id $ do
      get "openapi.json" $ json document
      defDocumentedEndpoint getItem $ \itemId offset requestedBy ->
        pure $ Item itemId (itemId + fromMaybe 0 offset) requestedBy
      defDocumentedEndpoint patchItem $ \itemId newValue -> pure $ Item itemId newValue Nothing
      defDocumentedEndpoint deleteItem $ \itemId -> pure (itemId > 0)
