{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Monads (app)
import Web.Spock
import Web.Spock.Config

main :: IO ()
main = do
  cfg <- defaultSpockCfg () PCNoDatabase "Hello"
  runSpock 8080 (spock cfg app)
