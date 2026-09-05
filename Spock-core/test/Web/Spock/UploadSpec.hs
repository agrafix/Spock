{-# LANGUAGE OverloadedStrings #-}

module Web.Spock.UploadSpec (spec) where

import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.Text as T
import Data.Word (Word64)
import Network.HTTP.Types.Status
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as WaiTest
import System.Directory (doesFileExist)
import Test.Hspec
import Web.Spock.Core

data CapturedUploads = CapturedUploads
  { contentByField :: HM.HashMap T.Text [(T.Text, T.Text, BS.ByteString)],
    legacyNames :: HM.HashMap T.Text T.Text,
    repeatedPathsMatch :: Bool,
    requestParams :: [(T.Text, T.Text)],
    originalBody :: BS.ByteString,
    temporaryPaths :: [FilePath]
  }

spec :: Spec
spec =
  describe "Multipart uploads" $
    do
      forM_ [False, True] $ \parseParamsFirst ->
        it ("keeps every file in upload order; params first = " ++ show parseParamsFirst) $
          withUploads parseParamsFirst sampleBody $ \captured ->
            HM.lookup "documents" (contentByField captured)
              `shouldBe` Just [("first.txt", "text/plain", "one"), ("second.txt", "text/plain", "two")]
      it "keeps distinct upload fields separate" $
        withUploads False sampleBody $ \captured ->
          HM.lookup "avatar" (contentByField captured) `shouldBe` Just [("avatar.txt", "text/plain", "image")]
      it "preserves the last file per field in the legacy API" $
        withUploads False sampleBody $ \captured ->
          legacyNames captured `shouldBe` HM.fromList [("documents", "second.txt"), ("avatar", "avatar.txt")]
      it "shares cached parsing with files, paramsPost, and body" $
        withUploads False sampleBody $ \captured ->
          do
            repeatedPathsMatch captured `shouldBe` True
            requestParams captured `shouldBe` [("description", "example")]
            originalBody captured `shouldBe` BSL.toStrict sampleBody
      it "removes every temporary upload after the request" $
        withUploads False sampleBody $ \captured ->
          do
            length (temporaryPaths captured) `shouldBe` 3
            mapM doesFileExist (temporaryPaths captured) `shouldReturn` [False, False, False]
      it "returns empty maps when there are no uploads" $
        withUploads False "--boundary--\r\n" $ \captured ->
          do
            contentByField captured `shouldBe` HM.empty
            legacyNames captured `shouldBe` HM.empty
      it "still enforces the request size limit for uploads" $
        do
          (res, _) <- requestUploads False (Just 10) sampleBody
          WaiTest.simpleStatus res `shouldBe` status413

sampleBody :: BSL.ByteString
sampleBody =
  filePart "documents" "first.txt" "one"
    <> filePart "avatar" "avatar.txt" "image"
    <> filePart "documents" "second.txt" "two"
    <> "--boundary\r\nContent-Disposition: form-data; name=\"description\"\r\n\r\nexample\r\n"
    <> "--boundary--\r\n"
  where
    filePart field filename content =
      "--boundary\r\nContent-Disposition: form-data; name=\"" <> field <> "\"; filename=\""
        <> filename <> "\"\r\nContent-Type: text/plain\r\n\r\n" <> content <> "\r\n"

withUploads :: Bool -> BSL.ByteString -> (CapturedUploads -> Expectation) -> Expectation
withUploads parseParamsFirst content check =
  do
    (res, captured) <- requestUploads parseParamsFirst Nothing content
    WaiTest.simpleStatus res `shouldBe` status200
    case captured of
      Just result -> check result
      Nothing -> expectationFailure "Upload handler did not capture the request"

requestUploads :: Bool -> Maybe Word64 -> BSL.ByteString -> IO (WaiTest.SResponse, Maybe CapturedUploads)
requestUploads parseParamsFirst maxSize content =
  do
    captured <- newIORef Nothing
    application <-
      spockAsApp $
        spockConfigT (defaultSpockConfig {sc_maxRequestSize = maxSize}) id $
          post root $
            do
              when parseParamsFirst $ void paramsPost
              grouped <- filesMulti
              single <- files
              again <- filesMulti
              postParams <- paramsPost
              raw <- body
              let paths = HM.map (map uf_tempLocation) grouped
                  readUpload uploaded =
                    do
                      contents <- BS.readFile (uf_tempLocation uploaded)
                      pure (uf_name uploaded, uf_contentType uploaded, contents)
              contents <- liftIO $ traverse (mapM readUpload) grouped
              liftIO $
                writeIORef captured $
                  Just $
                    CapturedUploads contents (HM.map uf_name single)
                      (paths == HM.map (map uf_tempLocation) again) postParams raw (concat $ HM.elems paths)
              text "ok"
    let req =
          Wai.defaultRequest
            { Wai.requestMethod = "POST",
              Wai.requestHeaders = [("Content-Type", "multipart/form-data; boundary=boundary")],
              Wai.requestBodyLength = Wai.KnownLength (fromIntegral $ BSL.length content)
            }
    res <- WaiTest.runSession (WaiTest.srequest $ WaiTest.SRequest req content) application
    (,) res <$> readIORef captured
