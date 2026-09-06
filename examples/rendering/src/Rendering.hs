{-# LANGUAGE OverloadedStrings #-}

module Rendering (app) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Lucid as L
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Text as RT
import qualified Text.Blaze.Html.Renderer.Utf8 as R
import Web.Spock.Core

app :: SpockT IO ()
app = do
  get root $ do
    message <- readMessage
    html $ TL.toStrict $ RT.renderHtml $ H.p (H.toHtml message)
  get "utf8" $ do
    message <- readMessage
    setHeader "Content-Type" "text/html; charset=utf-8"
    lazyBytes $ R.renderHtml $ H.p (H.toHtml message)
  get "lucid" $ do
    message <- readMessage
    setHeader "Content-Type" "text/html; charset=utf-8"
    lazyBytes $ L.renderBS $ L.p_ (L.toHtml message)

readMessage :: ActionT IO Text
readMessage = fromMaybe "Hello, λ!" <$> param "message"
