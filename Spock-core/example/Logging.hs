{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (bracket)
import Data.Aeson (encode, toJSON)
import System.Log.FastLogger
import Web.Spock.Core

main :: IO ()
main = bracket (newStdoutLoggerSet defaultBufSize) rmLoggerSet $ \logger -> do
  let logging = defaultLoggingConfig (pushLogStrLn logger . toLogStr . encode)
      config = defaultSpockConfig { sc_logging = Just logging }
  runSpock 8081 $ spockConfigT config id $ get root $ do
    logMessage LogInfo "serving home" [("example", toJSON True)]
    getRequestId >>= text . maybe "missing" id
