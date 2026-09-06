{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import BrowserServer
import Control.Monad (forM_)
import Data.Aeson (FromJSON, eitherDecode, encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Network.HTTP.Types
import qualified Network.Wai as W
import qualified Network.Wai.Test as W
import Shared
import Test.Hspec
import qualified Web.Cookie as C

main :: IO ()
main = hspec $ beforeAll (makeApp False "/unused-assets") $ describe "Shared browser API server" $ do
  it "requires the session cookie and CSRF token on all unsafe methods" $ \app -> do
    (_, headers) <- visitor app
    forM_ ["POST", "PUT", "PATCH", "DELETE"] $ \method -> do
      W.simpleStatus <$> send app method "/api/note" [] "\"hello\"" `shouldReturn` status403
      W.simpleStatus <$> send app method "/api/note" (filter ((/= "X-Csrf-Token") . fst) headers) "\"hello\"" `shouldReturn` status403
  it "supports POST 201, GET, PUT, PATCH and DELETE using the same session" $ \app -> do
    (cookie, headers) <- visitor app
    created <- send app "POST" "/api/note" headers (encode ("λ😀" :: T.Text))
    W.simpleStatus created `shouldBe` status201
    decode created `shouldReturn` ("λ😀" :: T.Text)
    send app "GET" "/api/note" [cookie] "" >>= decode >>= (`shouldBe` Just ("λ😀" :: T.Text))
    send app "PUT" "/api/note" headers "\"replacement\"" >>= decode >>= (`shouldBe` ("replacement" :: T.Text))
    send app "PATCH" "/api/note" headers "\" appended\"" >>= decode >>= (`shouldBe` ("replacement appended" :: T.Text))
    send app "DELETE" "/api/note" headers "" >>= decode >>= (`shouldBe` True)
    send app "GET" "/api/note" [cookie] "" >>= decode >>= (`shouldBe` (Nothing :: Maybe T.Text))
  it "isolates different browser sessions" $ \app -> do
    (cookie, headers) <- visitor app
    _ <- send app "POST" "/api/note" headers "\"private\""
    send app "GET" "/api/note" [] "" >>= decode >>= (`shouldBe` (Nothing :: Maybe T.Text))
    send app "GET" "/api/note" [cookie] "" >>= decode >>= (`shouldBe` Just ("private" :: T.Text))
  it "rejects missing, malformed and oversized notes without erasing the saved value" $ \app -> do
    (cookie, headers) <- visitor app
    W.simpleStatus <$> send app "PATCH" "/api/note" headers "\"hello\"" `shouldReturn` status404
    _ <- send app "POST" "/api/note" headers "\"saved\""
    forM_ ["\"\"", "not-json", "42", encode (T.replicate 201 "a")] $ \body ->
      W.simpleStatus <$> send app "PUT" "/api/note" headers body `shouldReturn` status400
    send app "GET" "/api/note" [cookie] "" >>= decode >>= (`shouldBe` Just ("saved" :: T.Text))
  it "round-trips typed extension paths, repeated queries and optional headers" $ \app -> do
    resp <- send app "GET" "/api/echo/report%2Fa.b%20%CE%BB%F0%9F%98%80.json?search=a%2Bb%26%CE%BB&offset=2&tag=first&tag=two%20words"
      [("X-Caller", "browser"), ("X-Optional", "optional")] ""
    decode resp `shouldReturn` Echo "report/a.b λ😀" "a+b&λ" (Just 2) ["first", "two words"] "browser" (Just "optional")
    filter ((== hSetCookie) . fst) (W.simpleHeaders resp) `shouldBe` []
  it "validates required, scalar and optional parameters" $ \app -> do
    forM_ [("", [("X-Caller", "browser")]), ("?search=q", []),
      ("?search=q&offset=bad", [("X-Caller", "browser")]),
      ("?search=q&offset=1&offset=2", [("X-Caller", "browser")])] $ \(query, headers) ->
        W.simpleStatus <$> send app "GET" ("/api/echo/name.json" <> query) headers "" `shouldReturn` status400
  it "does not expose arbitrary filesystem paths or unknown API routes" $ \app -> do
    forM_ ["/../Spock.cabal", "/api/unknown", "/server/app/Main.hs"] $ \path ->
      W.simpleStatus <$> send app "GET" path [] "" `shouldReturn` status404

send :: W.Application -> Method -> B.ByteString -> RequestHeaders -> BL.ByteString -> IO W.SResponse
send app method path headers body = W.runSession (W.srequest $ W.SRequest
  ((W.setPath W.defaultRequest path) { W.requestMethod = method, W.requestHeaders = headers }) body) app

visitor :: W.Application -> IO (Header, RequestHeaders)
visitor app = do
  resp <- send app "GET" "/api/csrf" [] ""
  token <- decode resp :: IO T.Text
  raw <- maybe (fail "Missing cookie") pure $ lookup hSetCookie $ W.simpleHeaders resp
  let cookie = C.parseSetCookie raw
      header = (hCookie, C.setCookieName cookie <> "=" <> C.setCookieValue cookie)
  pure (header, [header, ("X-Csrf-Token", stringBytes token), (hContentType, "application/json")])

stringBytes :: T.Text -> B.ByteString
stringBytes = B.pack . map (fromIntegral . fromEnum) . T.unpack

decode :: FromJSON a => W.SResponse -> IO a
decode = either fail pure . eitherDecode . W.simpleBody
