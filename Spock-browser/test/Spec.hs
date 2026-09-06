{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Exception (throwIO, AsyncException (ThreadKilled))
import Control.Monad (forM_)
import Data.HVect (HVect (..))
import Data.IORef
import qualified Data.Text as T
import Test.Hspec
import Web.Spock.Browser

main :: IO ()
main = hspec $ do
  describe "Browser URL parsing and generation" $ do
    it "keeps encoded slashes inside captures and preserves query and fragment" $
      parseLocation "/app/a%2Fb%20%CE%BB%F0%9F%98%80?x=a+b#part?x" `shouldBe`
        Right (Location "/app/a%2Fb%20%CE%BB%F0%9F%98%80" ["app", "a/b λ😀"] "?x=a+b" "#part?x")
    it "decodes exactly once and does not treat path plus as space" $
      locationSegments <$> parseLocation "/app/%252F+a" `shouldBe` Right ["app", "%2F+a"]
    it "preserves empty segments and distinguishes root" $ do
      locationSegments <$> parseLocation "/a//b/" `shouldBe` Right ["a", "", "b", ""]
      locationSegments <$> parseLocation "/?q#f" `shouldBe` Right []
    it "rejects external URLs, raw controls and browser-normalized dot segments" $
      forM_ ["", "relative", "https://example.test/app", "//example.test", "/a b", "/a\r", "/\\evil",
        "/a/../b", "/%2E", "/%2e%2E", "/a/./b"] $ \url -> parseLocation url `shouldBe` Left InvalidLocation
    it "rejects malformed percent encodings and invalid UTF-8" $
      forM_ ["/%", "/%2", "/%GG", "/%FF", "/%C0%AF", "/%ED%A0%80"] $ \url -> parseLocation url `shouldBe` Left InvalidLocation
    it "renders typed Unicode paths and extensions for round trips" $ do
      let path = "app" <//> var <.> "json" :: Path '[T.Text] 'Open
      renderPath IgnoreSlashes path ("a/b λ😀" :&: HNil) `shouldBe` Right "/app/a%2Fb%20%CE%BB%F0%9F%98%80.json"
    it "renders root and strict trailing slashes" $ do
      renderPath IgnoreSlashes root HNil `shouldBe` Right "/"
      renderPath StrictSlashes (trailingSlash $ "app" <//> var) ((3 :: Int) :&: HNil) `shouldBe` Right "/app/3/"
    it "rejects captures that would escape the rendered origin or normalize away" $
      forM_ ["", ".", ".."] $ \value ->
        renderPath IgnoreSlashes (var <//> "app") ((value :: T.Text) :&: HNil) `shouldBe` Left InvalidLocation
  describe "Typed browser route dispatch" $ do
    it "parses captured values before selecting a route" $ do
      values <- newIORef []
      router <- compileRoutes IgnoreSlashes $ route ("app" <//> (var :: Path '[Int] 'Open)) (\value -> modifyIORef' values (++ [value]))
      run router "/app/42?x=1#section" `shouldReturn` True
      run router "/app/not-an-int" `shouldReturn` False
      readIORef values `shouldReturn` [42]
    it "dispatches Unicode and encoded-slash captures without extra segments" $ do
      values <- newIORef []
      router <- compileRoutes IgnoreSlashes $ route ("app" <//> (var :: Path '[T.Text] 'Open)) (\value -> modifyIORef' values (++ [value]))
      run router "/app/a%2Fb%20%CE%BB" `shouldReturn` True
      readIORef values `shouldReturn` ["a/b λ"]
    it "runs only the most specific match, then extensions, then a wildcard" $ do
      values <- newIORef ([] :: [T.Text])
      let record = modifyIORef' values . flip (++) . (: [])
      router <- compileRoutes IgnoreSlashes $ do
        route ("app" <//> wildcard) (\_ -> record "wildcard")
        route ("app" <//> (var :: Path '[T.Text] 'Open) <.> "json") (\_ -> record "extension")
        route "app/fixed.json" (record "static")
      forM_ ["/app/fixed.json", "/app/other.json", "/app/a/b"] $ \url -> run router url `shouldReturn` True
      readIORef values `shouldReturn` ["static", "extension", "wildcard"]
    it "applies IgnoreSlashes consistently to registry and incoming locations" $ do
      router <- compileRoutes IgnoreSlashes $ route "app//about/" (pure ())
      run router "/app/about" `shouldReturn` True
      run router "/app///about/" `shouldReturn` True
    it "preserves internal and trailing empty segments in strict mode" $ do
      router <- compileRoutes StrictSlashes $ route "app//about/" (pure ())
      run router "/app//about/" `shouldReturn` True
      run router "/app/about/" `shouldReturn` False
      run router "/app//about" `shouldReturn` False
    it "returns a miss without running handlers for an unknown path" $ do
      router <- compileRoutes IgnoreSlashes $ route "app/about" (fail "unexpected handler")
      run router "/app/unknown" `shouldReturn` False
    it "preserves handler cancellation" $ do
      router <- compileRoutes IgnoreSlashes $ route root (throwIO ThreadKilled)
      run router "/" `shouldThrow` (== ThreadKilled)

run :: Router -> T.Text -> IO Bool
run router url = either (fail . show) (dispatch router) (parseLocation url)
