{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp
import People (withPeopleApp)
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [database, value] | Just port <- readMaybe value, port > 0 && port <= 65535 ->
      withPeopleApp (T.pack database) $
        Warp.runSettings (Warp.setHost "127.0.0.1" $ Warp.setPort port Warp.defaultSettings)
    _ -> fail "Usage: spock-rest-example DATABASE PORT"
