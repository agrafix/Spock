{-# LANGUAGE OverloadedStrings #-}
module Web.Spock.Specs.SimpleSpec where

import Web.Spock.Simple
import Web.Spock.Specs.FrameworkSpecHelper

import Test.Hspec

app :: SpockT IO ()
app =
    do get "/" $ text "root"

spec :: Spec
spec = frameworkSpec (spockApp id app)
