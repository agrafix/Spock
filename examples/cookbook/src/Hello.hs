{-# LANGUAGE OverloadedStrings #-}

module Hello (app) where

import Network.Wai (Middleware)
import Web.Spock
import Web.Spock.Config

app :: IO Middleware
app = do
  cfg <- defaultSpockCfg () PCNoDatabase ()
  spock cfg routes

routes :: SpockM () () () ()
routes = do
  get root $ text "Hello World!"
  get ("hello" <//> var) $ \name -> text ("Hello " <> name)
