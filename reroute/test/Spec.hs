module Main where

import Test.Hspec
import qualified Web.Routing.SafeRoutingSpec

main :: IO ()
main =
  hspec $
    Web.Routing.SafeRoutingSpec.spec
