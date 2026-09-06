module Main (main) where

import qualified CookbookSpec
import qualified HelloSpec
import Test.Hspec

main :: IO ()
main = hspec $ HelloSpec.spec >> CookbookSpec.spec
