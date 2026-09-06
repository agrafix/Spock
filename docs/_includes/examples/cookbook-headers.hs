get "headers" $ do
  client <- header "X-Client"
  requestId <- getRequestId
  setHeader "X-Reply" "received"
  logMessage LogInfo "Read headers" []
  json $ object ["client" .= client, "requestId" .= requestId]
