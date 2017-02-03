module Main where

import qualified Web.Spock.CsrfSpec
import qualified Web.Spock.Internal.SessionManagerSpec
import qualified Web.Spock.Internal.SessionVaultSpec
import qualified Web.Spock.SafeSpec

import Test.Hspec

main :: IO ()
main = hspec $
    do Web.Spock.Internal.SessionVaultSpec.spec
       Web.Spock.Internal.SessionManagerSpec.spec
       Web.Spock.SafeSpec.spec
       Web.Spock.CsrfSpec.spec
