module Main where

import qualified Data.PolyMapSpec
import Test.Hspec
import qualified Web.Routing.SafeRoutingSpec

main :: IO ()
main =
  hspec $
    do
      Data.PolyMapSpec.spec
      Web.Routing.SafeRoutingSpec.spec
