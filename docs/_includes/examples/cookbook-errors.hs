errorJson :: MonadIO m => Status -> T.Text -> ActionCtxT ctx m a
errorJson status message = do
  setStatus status
  json (object ["error" .= object ["status" .= statusCode status, "message" .= message]] :: Value)
