module Main where

import Test.Hspec
import qualified Web.Spock.Internal.CookiesSpec
import qualified Web.Spock.Internal.UtilSpec
import qualified Web.Spock.SafeSpec

main :: IO ()
main = hspec $
  do
    Web.Spock.Internal.CookiesSpec.spec
    Web.Spock.Internal.UtilSpec.spec
    Web.Spock.SafeSpec.spec
