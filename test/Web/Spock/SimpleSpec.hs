{-# LANGUAGE OverloadedStrings #-}
module Web.Spock.SimpleSpec where

import Web.Spock.Simple
import Web.Spock.Internal.FrameworkSpecHelper

import Test.Hspec

app :: SpockT IO ()
app =
    do get "/" $ text "root"

spec :: Spec
spec = frameworkSpec (spockApp id app)
