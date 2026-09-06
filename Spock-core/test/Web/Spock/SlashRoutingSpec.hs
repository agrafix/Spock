{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Spock.SlashRoutingSpec (spec) where

import qualified Data.Text as T
import qualified Data.ByteString as BS
import Network.HTTP.Types.Status (status308, status404)
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as WaiTest
import Test.Hspec
import qualified Test.Hspec.Wai as Test
import Web.Spock.Core

spec :: Spec
spec = describe "Slash routing" $ do
  Test.with (makeApp StrictSlashes) $ do
    it "distinguishes a path with a trailing slash" $ do
      Test.get "/foo" `Test.shouldRespondWith` "file"
      Test.get "/foo/" `Test.shouldRespondWith` "directory"
    it "matches root and rejects extra slashes" $ do
      Test.get "/" `Test.shouldRespondWith` "root"
      Test.get "/foo//" `Test.shouldRespondWith` 404
    it "retains repeated internal slashes only when explicitly registered" $ do
      Test.get "/a//b" `Test.shouldRespondWith` "repeated"
      Test.get "/a/b" `Test.shouldRespondWith` 404
    it "preserves the trailing slash after typed captures" $ do
      Test.get "/item/42/" `Test.shouldRespondWith` "42"
      Test.get "/item/42" `Test.shouldRespondWith` 404
    it "retains wildcard remainder slashes" $ do
      Test.get "/files/" `Test.shouldRespondWith` ""
      Test.get "/files/a//b/" `Test.shouldRespondWith` "a//b/"
    it "matches decoded Unicode and encoded separators as one capture" $ do
      Test.get "/name/%CE%BB%2Fspace%20here/" `Test.shouldRespondWith` "λ/space here"

  Test.with (makeApp IgnoreSlashes) $
    it "keeps the historical normalization by default" $ do
      Test.get "/foo" `Test.shouldRespondWith` "directory"
      Test.get "/foo//" `Test.shouldRespondWith` "directory"
      Test.get "/a/b" `Test.shouldRespondWith` "repeated"
      Test.get "/item/42" `Test.shouldRespondWith` "42"

  Test.with (makeApp RedirectTrailingSlashes) $ do
    it "redirects only when the alternate registered route matches" $ do
      Test.get "/directory" `Test.shouldRespondWith` 308 { Test.matchHeaders = ["Location" Test.<:> "/directory/"] }
      Test.get "/file/" `Test.shouldRespondWith` 308 { Test.matchHeaders = ["Location" Test.<:> "/file"] }
      Test.get "/missing" `Test.shouldRespondWith` 404
      Test.get "/directory//child" `Test.shouldRespondWith` 404
    it "leaves matching routes, root, and wildcard routes alone" $ do
      Test.get "/foo" `Test.shouldRespondWith` "file"
      Test.get "/foo/" `Test.shouldRespondWith` "directory"
      Test.get "/" `Test.shouldRespondWith` "root"
      Test.get "/files/one/" `Test.shouldRespondWith` "one/"
    it "preserves the method and body with a 308 and runs the handler only after following it" $ do
      Test.request "POST" "/echo" [] "payload" `Test.shouldRespondWith` 308 { Test.matchHeaders = ["Location" Test.<:> "/echo/"] }
      Test.request "POST" "/echo/" [] "payload" `Test.shouldRespondWith` "payload"
      Test.request "DELETE" "/echo" [] "" `Test.shouldRespondWith` 404

  it "preserves percent-encoding and the raw query in both directions" $ do
    app <- makeApp RedirectTrailingSlashes
    first <- sendRaw app "/name/%ce%bb%2fspace%20here?next=%2F&x=1+2&x=3"
    WaiTest.simpleStatus first `shouldBe` status308
    lookup "Location" (WaiTest.simpleHeaders first) `shouldBe` Just "/name/%ce%bb%2fspace%20here/?next=%2F&x=1+2&x=3"
    second <- sendRaw app "/file/?q=%2f+%20"
    WaiTest.simpleStatus second `shouldBe` status308
    lookup "Location" (WaiTest.simpleHeaders second) `shouldBe` Just "/file?q=%2f+%20"

  it "does not redirect scheme-relative or backslash paths" $ do
    app <- makeApp RedirectTrailingSlashes
    mapM_ (\path -> do
      response <- sendRaw app path
      WaiTest.simpleStatus response `shouldBe` status404
      lookup "Location" (WaiTest.simpleHeaders response) `shouldBe` Nothing)
      ["//example.com", "/\\example.com"]

  it "leaves a matching fallback in control" $ do
    app <- spockAsApp $ spockConfigT (defaultSpockConfig { sc_slashPolicy = RedirectTrailingSlashes }) id $ do
      get "dir/" $ text "directory"
      hookAny GET $ const $ text "fallback"
    response <- WaiTest.runSession (WaiTest.request $ WaiTest.setPath Wai.defaultRequest "/dir") app
    WaiTest.simpleBody response `shouldBe` "fallback"

  it "renders static and captured paths using the selected policy" $ do
    renderRoute "/foo/" `shouldBe` "/foo"
    renderRouteWith StrictSlashes "/foo/" `shouldBe` "/foo/"
    renderRouteWith StrictSlashes "a//b" `shouldBe` "/a//b"
    renderRouteWith StrictSlashes root `shouldBe` "/"
    renderRouteWith StrictSlashes (trailingSlash $ "item" <//> (var :: Var Int)) 42 `shouldBe` "/item/42/"
    renderRouteWith StrictSlashes (trailingSlash root) `shouldBe` "/"
    renderRouteWith StrictSlashes (trailingSlash "foo/") `shouldBe` "/foo/"
    renderRouteWith StrictSlashes ("item/" <//> (var :: Var Int)) 42 `shouldBe` "/item//42"

makeApp :: SlashPolicy -> IO Wai.Application
makeApp policy = spockAsApp $ spockConfigT (defaultSpockConfig { sc_slashPolicy = policy }) id $ do
  get root $ text "root"
  get "foo" $ text "file"
  get "foo/" $ text "directory"
  get "directory/" $ text "directory"
  get "file" $ text "file"
  get "a//b" $ text "repeated"
  get (trailingSlash $ "item" <//> var) $ \(n :: Int) -> text $ T.pack $ show n
  get (trailingSlash $ "name" <//> var) $ text
  get ("files" <//> wildcard) $ text
  get "//example.com/" $ text "explicit repeated leading slash"
  get "/\\example.com/" $ text "explicit backslash"
  post "echo/" $ body >>= bytes

-- hspec-wai's request helper re-encodes paths and queries. Set raw fields
-- explicitly to exercise the bytes a server receives from the client.
sendRaw :: Wai.Application -> BS.ByteString -> IO WaiTest.SResponse
sendRaw app target = WaiTest.runSession (WaiTest.request req) app
  where
    (path, query) = BS.break (== 63) target
    req = (WaiTest.setPath Wai.defaultRequest target)
      { Wai.rawPathInfo = path, Wai.rawQueryString = query }
