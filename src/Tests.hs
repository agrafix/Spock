{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Tests.Routing
import {-@ HTF_TESTS @-} Tests.Wire

main :: IO ()
main = htfMain htf_importedTests
