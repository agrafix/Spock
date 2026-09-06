module Main (main) where

import Rendering (app)
import Web.Spock.Core

main :: IO ()
main = runSpock 8080 (spockT id app)
