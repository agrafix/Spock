module Web.Spock.Specs.All where

import qualified Web.Spock.Specs.SimpleSpec
import qualified Web.Spock.Specs.SafeSpec
import qualified Web.Spock.Specs.TextRoutingSpec

import Test.Hspec

allSpecs :: Spec
allSpecs =
    do Web.Spock.Specs.SimpleSpec.spec
       Web.Spock.Specs.SafeSpec.spec
       Web.Spock.Specs.TextRoutingSpec.spec
