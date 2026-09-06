{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import BrowserServer
import qualified Network.Wai.Handler.Warp as Warp
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  (secure, port, assets) <- case args of
    [mode, portText, directory] | Just port <- readMaybe portText, port >= 1, port <= 65535,
      Just secure <- lookup mode [("--local-http", False), ("--https", True)] -> pure (secure, port, directory)
    _ -> fail "Usage: spock-browser-server (--local-http|--https) PORT ASSET_DIRECTORY"
  app <- makeApp secure assets
  hPutStrLn stderr $ "Listening on 127.0.0.1:" ++ show port
  Warp.runSettings (Warp.setHost "127.0.0.1" $ Warp.setPort port Warp.defaultSettings) app
