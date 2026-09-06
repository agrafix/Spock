post "json" $ do
  value <- jsonBody :: Action (Maybe T.Text)
  case value of
    Nothing -> errorJson status400 "Expected a JSON string"
    Just message -> json $ object ["message" .= message]
