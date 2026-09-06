{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Web.HttpApiData
import qualified Web.Scotty as Scotty
import qualified Web.Spock as Spock
import Web.Spock.Config
import qualified Web.Spock.Core as Core

newtype Digits = Digits T.Text

instance FromHttpApiData Digits where
  parseUrlPiece value
    | not (T.null value) && T.all (\c -> c >= '0' && c <= '9') value = Right $ Digits value
    | otherwise = Left "Expected decimal digits"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [mode, portText] | Just port <- readMaybe portText, port > 0 -> do
      app <- case mode of
        "warp" -> pure bare
        "core" -> Core.spockAsApp $ Core.spockT id routes
        "scotty" -> Scotty.scottyApp $ do
          Scotty.get "/echo/hello-world" $ Scotty.text "Hello World"
          Scotty.get "/echo/plain/:value" $ Scotty.pathParam "value" >>= Scotty.text
          Scotty.get (Scotty.regex "^/echo/regex/([0-9]+)$") $
            Scotty.pathParam "1" >>= Scotty.text
        _ | Just sessionMode <- lookup mode
              [("default", Nothing), ("always", Just SessionsAlways),
               ("on-demand", Just SessionsOnDemand), ("disabled", Just SessionsDisabled)] -> do
          cfg <- defaultSpockCfg () PCNoDatabase ()
          let sessions = spc_sessionCfg cfg
              selected = maybe sessions (\m -> sessions { sc_sessionMode = m }) sessionMode
          Spock.spockAsApp $ Spock.spock (cfg { spc_sessionCfg = selected }) routes
        _ -> fail "Unknown benchmark mode"
      Warp.runSettings (Warp.setHost "127.0.0.1" $ Warp.setPort port Warp.defaultSettings) app
    _ -> fail "Usage: spock-http-bench MODE PORT [+RTS -N2 -RTS]"

routes :: MonadIO m => Core.SpockCtxT () m ()
routes = do
  Core.get ("echo" Core.<//> "hello-world") $ Core.text "Hello World"
  Core.get ("echo" Core.<//> "plain" Core.<//> Core.var) $ \(value :: T.Text) -> Core.text value
  Core.get ("echo" Core.<//> "regex" Core.<//> Core.var) $ \(Digits value) -> Core.text value

bare :: Wai.Application
bare req respond = respond $ case (Wai.requestMethod req, Wai.pathInfo req) of
  ("GET", ["echo", "hello-world"]) -> ok "Hello World"
  ("GET", ["echo", "plain", value]) -> ok value
  ("GET", ["echo", "regex", value]) | Right (Digits digits) <- parseUrlPiece value -> ok digits
  _ -> Wai.responseLBS status404 [] "Not found"
  where
    ok value = Wai.responseLBS status200 [(hContentType, "text/plain; charset=utf-8")]
      (LBS.fromStrict $ T.encodeUtf8 value)
