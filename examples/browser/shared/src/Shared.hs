{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Shared where

import Data.Aeson
import Data.Text (Text)
import Web.Spock.Api
import Web.Spock.Api.Document

getCsrf :: Endpoint '[] 'Nothing Text
getCsrf = MethodGet "api/csrf"

getNote :: Endpoint '[] 'Nothing (Maybe Text)
getNote = MethodGet "api/note"

createNote, replaceNote, appendNote :: Endpoint '[] ('Just Text) Text
createNote = MethodPost Proxy "api/note"
replaceNote = MethodPut Proxy "api/note"
appendNote = MethodPatch Proxy "api/note"

deleteNote :: Endpoint '[] 'Nothing Bool
deleteNote = MethodDelete "api/note"

data Echo = Echo
  { echoName :: Text, echoSearch :: Text, echoOffset :: Maybe Int,
    echoTags :: [Text], echoCaller :: Text, echoOptional :: Maybe Text }
  deriving (Eq, Show, Generic)
instance ToJSON Echo
instance FromJSON Echo

-- One definition carries path/query/header types for both native server and JS.
echo :: DocumentedEndpoint '[Text] '[Text, Maybe Int, [Text], Text, Maybe Text] 'Nothing Echo
echo = DocumentedEndpoint (MethodGet ("api/echo" <//> (var <.> "json")))
  (operationInfo "echo") (PathParameter (parameterInfo "name" textSchema) NoPathParameters)
  (QueryParam (parameterInfo "search" textSchema) :>
   OptionalQueryParam (parameterInfo "offset" intSchema) :>
   QueryList (parameterInfo "tag" textSchema) :>
   HeaderParam (parameterInfo "X-Caller" textSchema) :>
   OptionalHeaderParam (parameterInfo "X-Optional" textSchema) :> NoParameters)
  NoBody (schemaObject [("type", String "object")])
