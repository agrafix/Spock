{-# LANGUAGE OverloadedStrings #-}

module Cookbook (makeApp) where

import Control.Monad (forM, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Network.HTTP.Types
import qualified Network.Wai as Wai
import Network.Wai.Middleware.Cors
import Web.Spock
import Web.Spock.Config

type Action a = SpockAction () () () a

makeApp :: (LogEvent -> IO ()) -> IO Wai.Application
-- example: configuration
makeApp sink = do
  cfg <- defaultSpockCfg () PCNoDatabase ()
  let sessions = (spc_sessionCfg cfg) { sc_sessionMode = SessionsDisabled }
  spockAsApp $ spock (cfg
    { spc_sessionCfg = sessions,
      spc_maxRequestSize = Just (1024 * 1024),
      spc_logging = Just $ defaultLoggingConfig sink,
      spc_logError = const $ pure (), -- The structured sink receives diagnostics.
      spc_errorHandler = \status -> errorJson status "Request failed" }) routes
-- end-example: configuration

routes :: SpockM () () () ()
routes = do
  -- example: middleware
  -- Middleware wraps requests and responses, including requests with no route.
  middleware $ \application req respond -> application req $
    respond . Wai.mapResponseHeaders (("X-Cookbook", "Spock") :)
  -- Public stateless demo: allow one development origin, without credentials.
  middleware $ cors $ const $ Just simpleCorsResourcePolicy
    { corsOrigins = Just (["http://localhost:3000"], False),
      corsMethods = ["GET", "POST", "OPTIONS"],
      corsRequestHeaders = ["Content-Type", "X-Client"] }
  -- end-example: middleware
  -- example: headers
  get "headers" $ do
    client <- header "X-Client"
    requestId <- getRequestId
    setHeader "X-Reply" "received"
    logMessage LogInfo "Read headers" []
    json $ object ["client" .= client, "requestId" .= requestId]
  -- end-example: headers
  -- example: json
  post "json" $ do
    value <- jsonBody :: Action (Maybe T.Text)
    case value of
      Nothing -> errorJson status400 "Expected a JSON string"
      Just message -> json $ object ["message" .= message]
  -- end-example: json
  -- example: form
  post "form" $ do
    fields <- paramsPost
    case [value | (name, value) <- fields, name == "name"] of
      [name] -> json $ object ["name" .= name]
      _ -> errorJson status400 "Expected exactly one name field"
  -- end-example: form
  -- example: upload
  post "upload" $ do
    uploads <- filesMulti
    let allFiles = [(field, upload) | (field, values) <- HM.toList uploads, upload <- values]
    when (null allFiles) $ errorJson status400 "No files uploaded"
    summaries <- forM allFiles $ \(field, upload) -> do
      -- Read inside the action, before Spock removes its temporary files.
      -- The request-size limit bounds this demonstration's in-memory reads.
      contents <- liftIO $ BS.readFile (uf_tempLocation upload)
      pure $ object ["field" .= field, "name" .= uf_name upload, "bytes" .= BS.length contents]
    setStatus status201
    json summaries
  -- end-example: upload
  get "failure" $ liftIO $ ioError $ userError "cookbook diagnostic: intentional failure"

-- A generic helper works in both ordinary actions and the IO error handler.
-- The handler receives a Status, not the exception or an application's state.
-- example: errors
errorJson :: MonadIO m => Status -> T.Text -> ActionCtxT ctx m a
errorJson status message = do
  setStatus status
  json (object ["error" .= object ["status" .= statusCode status, "message" .= message]] :: Value)
-- end-example: errors
