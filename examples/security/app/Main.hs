{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import BrowserSecurity
import Control.Exception (bracket)
import Control.Monad (when)
import Data.Password.Argon2
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.Wai.Handler.Warp as Warp
import System.Environment (getArgs)
import System.IO
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  (deployment, port) <- case args of
    [mode, portText]
      | Just port <- readMaybe portText,
        port >= 1 && port <= 65535,
        Just deployment <- lookup mode [("--local-http", LocalHTTP), ("--https", BehindHTTPS)] -> pure (deployment, port)
    _ -> fail "Usage: spock-security-example (--local-http|--https) PORT"
  hPutStr stderr "Choose a password for the demo account: "
  hFlush stderr
  terminal <- hIsTerminalDevice stdin
  password <- if terminal
    then bracket (hGetEcho stdin <* hSetEcho stdin False) (hSetEcho stdin) (const T.getLine)
    else T.getLine
  hPutStrLn stderr ""
  when (T.null password || T.length password > 1024) $ fail "Use a nonempty password of at most 1024 characters"
  passwordHash <- hashPassword (mkPassword password)
  application <- makeApp deployment passwordHash
  -- The local demo and the backend behind a TLS proxy both bind to loopback.
  hPutStrLn stderr $ "Listening on 127.0.0.1:" ++ show port
  Warp.runSettings (Warp.setHost "127.0.0.1" $ Warp.setPort port Warp.defaultSettings) application
