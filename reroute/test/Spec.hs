module Main where

import qualified Web.Routing.SafeRoutingSpec

import Test.Hspec

main :: IO ()
main = hspec $
    Web.Routing.SafeRoutingSpec.spec
