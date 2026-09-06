{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cookbook (makeApp)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Network.Wai.Handler.Warp as Warp
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  port <- case args of
    [value] | Just port <- readMaybe value, port > 0 && port <= 65535 -> pure port
    _ -> fail "Usage: spock-cookbook-example PORT"
  application <- makeApp (BL.putStrLn . encode)
  Warp.runSettings (Warp.setHost "127.0.0.1" $ Warp.setPort port Warp.defaultSettings) application
