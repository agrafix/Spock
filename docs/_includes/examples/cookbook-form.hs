post "form" $ do
  fields <- paramsPost
  case [value | (name, value) <- fields, name == "name"] of
    [name] -> json $ object ["name" .= name]
    _ -> errorJson status400 "Expected exactly one name field"
