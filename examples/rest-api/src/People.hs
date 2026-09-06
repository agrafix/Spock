{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module People (withPeopleApp) where

import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Data.Aeson (FromJSON, Value, object, (.=))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Database.Persist as P
import Database.Persist.Sqlite
  ( SqlBackend, SqlPersistT, fromSqlKey, toSqlKey, runSqlConn, runSqlPool,
    runMigrationQuiet, withSqlitePool )
import Database.Persist.TH
import GHC.Generics (Generic)
import qualified Network.Wai as Wai
import Network.HTTP.Types
import Web.Spock
import Web.Spock.Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
  name Text
  age Int
  deriving Show
|]

-- Keep the HTTP representation independent of generated database records.
data PersonInput = PersonInput { name :: Text, age :: Int }
  deriving stock (Show, Generic)
instance FromJSON PersonInput

type ApiAction a = SpockAction SqlBackend () () a

withPeopleApp :: Text -> (Wai.Application -> IO a) -> IO a
withPeopleApp database use = runNoLoggingT $ withSqlitePool database 1 $ \pool -> do
  -- Suitable for this new demo database. Review production migrations before
  -- applying them, and run them once as part of deployment.
  void $ runSqlPool (runMigrationQuiet migrateAll) pool
  cfg <- liftIO $ defaultSpockCfg () (PCPool pool) ()
  let sessions = (spc_sessionCfg cfg) { sc_sessionMode = SessionsDisabled }
  application <- liftIO $ spockAsApp $ spock (cfg
    { spc_sessionCfg = sessions,
      spc_maxRequestSize = Just (16 * 1024),
      spc_errorHandler = \status -> errorJson status "Request failed" }) routes
  liftIO $ use application

routes :: SpockM SqlBackend () () ()
routes = do
  post "people" $ do
    person <- readPerson
    key <- runSQL $ P.insert person
    setStatus status201
    setHeader "Location" $ "/people/" <> T.pack (show $ fromSqlKey key)
    json $ object ["result" .= ("success" :: Text), "id" .= fromSqlKey key]
  get "people" $ do
    people <- runSQL $ P.selectList [] [P.Asc PersonId]
    json $ map (\(P.Entity key person) -> personJSON (fromSqlKey key) person) people
  get ("people" <//> var) $ \(personId :: Int64) -> do
    found <- runSQL $ P.get (toSqlKey personId)
    maybe (errorJson status404 "Person not found") (json . personJSON personId) found
  put ("people" <//> var) $ \(personId :: Int64) -> do
    person <- readPerson
    changed <- runSQL $ do
      let key = toSqlKey personId
      found <- P.get key
      case found of
        Nothing -> pure False
        Just (_ :: Person) -> P.replace key person >> pure True
    if changed then json $ personJSON personId person else errorJson status404 "Person not found"
  delete ("people" <//> var) $ \(personId :: Int64) -> do
    removed <- runSQL $ do
      let key = toSqlKey personId
      found <- P.get key
      case found of
        Nothing -> pure False
        Just (_ :: Person) -> P.delete key >> pure True
    if removed then setStatus status204 >> text "" else errorJson status404 "Person not found"

readPerson :: ApiAction Person
readPerson = do
  parsed <- jsonBody :: ApiAction (Maybe PersonInput)
  case parsed of
    Nothing -> errorJson status400 "Expected a person with name and age"
    Just input -> do
      when (T.null (T.strip $ name input) || T.length (name input) > 100 || age input < 0 || age input > 130) $
        errorJson status400 "Name must have 1 to 100 characters and age must be 0 to 130"
      pure $ Person (name input) (age input)

runSQL :: SqlPersistT (NoLoggingT IO) a -> ApiAction a
runSQL query = runQuery $ \connection -> runNoLoggingT $ runSqlConn query connection

personJSON :: Int64 -> Person -> Value
personJSON personId person = object
  ["id" .= personId, "name" .= personName person, "age" .= personAge person]

errorJson :: MonadIO m => Status -> Text -> ActionCtxT ctx m a
errorJson status message = do
  setStatus status
  json $ object ["error" .= object ["status" .= statusCode status, "message" .= message]]
