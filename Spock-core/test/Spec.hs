module Main where

import Test.Hspec
import qualified Web.Spock.Internal.CookiesSpec
import qualified Web.Spock.Internal.UtilSpec
import qualified Web.Spock.SafeSpec
import qualified Web.Spock.UploadSpec
import qualified Web.Spock.LoggingSpec
import qualified Web.Spock.RequestSizeSpec
import qualified Web.Spock.SlashRoutingSpec

main :: IO ()
main = hspec $
  do
    Web.Spock.Internal.CookiesSpec.spec
    Web.Spock.Internal.UtilSpec.spec
    Web.Spock.SafeSpec.spec
    Web.Spock.UploadSpec.spec
    Web.Spock.LoggingSpec.spec
    Web.Spock.RequestSizeSpec.spec
    Web.Spock.SlashRoutingSpec.spec
