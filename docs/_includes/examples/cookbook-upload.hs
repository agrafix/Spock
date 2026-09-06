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
