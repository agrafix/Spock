{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Spock.ExtensionRoutingSpec (spec) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as WaiTest
import Test.Hspec
import qualified Test.Hspec.Wai as Test
import Web.HttpApiData
import Web.Spock.Core

data Format = Html | Txt deriving (Eq, Show)
instance FromHttpApiData Format where
  parseUrlPiece "html" = Right Html
  parseUrlPiece "txt" = Right Txt
  parseUrlPiece _ = Left "Expected html or txt"
instance ToHttpApiData Format where
  toUrlPiece Html = "html"
  toUrlPiece Txt = "txt"

spec :: Spec
spec = describe "Extension routing" $ do
  Test.with (makeApp IgnoreSlashes) $ do
    it "matches and parses a fixed extension on an integer capture" $
      Test.get "/pages/12.txt" `Test.shouldRespondWith` "12"
    it "rejects missing or incorrect suffixes and invalid typed captures" $
      mapM_ (\path -> Test.get path `Test.shouldRespondWith` 404)
        ["/pages/12", "/pages/12.", "/pages/12.html", "/pages/abc.txt", "/pages/12.txt.gz", "/pages/12.3.txt"]
    it "captures an extension using its own FromHttpApiData instance" $ do
      Test.get "/typed/12.html" `Test.shouldRespondWith` "12:Html"
      Test.get "/typed/12.txt" `Test.shouldRespondWith` "12:Txt"
      Test.get "/typed/12.xml" `Test.shouldRespondWith` 404
      Test.get "/typed/12." `Test.shouldRespondWith` 404
    it "keeps dots in the basename and supports multi-dot literal extensions" $ do
      Test.get "/names/report.v2.final.txt" `Test.shouldRespondWith` "report.v2.final"
      Test.get "/archives/report.v2.tar.gz" `Test.shouldRespondWith` "report.v2"
      Test.get "/archives/report.v2.gz" `Test.shouldRespondWith` 404
    it "supports multiple typed captures inside the same segment" $ do
      Test.get "/compressed/12.txt.gz" `Test.shouldRespondWith` "12:Txt"
      Test.get "/compressed/12.xml.gz" `Test.shouldRespondWith` 404
    it "keeps captures before and after the extension in handler order" $
      Test.get "/values/key/pages/12.txt/details/3" `Test.shouldRespondWith` "key:12:3"
    it "prefers static routes, then fixed suffixes, then extension captures, then plain variables" $ do
      Test.get "/priority/special.txt" `Test.shouldRespondWith` "static"
      Test.get "/priority/other.txt" `Test.shouldRespondWith` "fixed"
      Test.get "/priority/other.html" `Test.shouldRespondWith` "extension"
      Test.get "/priority/no-extension" `Test.shouldRespondWith` "plain"
    it "can append a wildcard after an extension" $
      Test.get "/rest/12.txt/a/b" `Test.shouldRespondWith` "12:a/b"
    it "preserves ordinary route fallthrough for extension handlers" $
      Test.get "/next/12.txt" `Test.shouldRespondWith` "12"

  Test.with (makeApp StrictSlashes) $
    it "retains trailing slashes on fixed and captured extensions" $ do
      Test.get "/directory/12.txt/" `Test.shouldRespondWith` "12:Txt"
      Test.get "/directory/12.txt" `Test.shouldRespondWith` 404

  it "round-trips reserved characters and Unicode with encoded rendering" $ do
    app <- makeApp StrictSlashes
    mapM_ (\name -> do
      let url = renderRouteEncoded ("names" <//> (var :: Var T.Text) <.> "txt") name
      result <- WaiTest.runSession (WaiTest.request $ WaiTest.setPath Wai.defaultRequest $ T.encodeUtf8 url) app
      WaiTest.simpleBody result `shouldBe` LBS.fromStrict (T.encodeUtf8 name))
      ["hello world", "λ/東京", "a?b#c%d", "a+b", "report.v2"]
    renderRouteEncoded ("names" <//> (var :: Var T.Text) <.> "txt") "a/b?c"
      `shouldBe` "/names/a%2Fb%3Fc.txt"

  it "renders captures, fixed suffixes, multiple extensions, and slash policies" $ do
    renderRoute ("pages" <//> (var :: Var Int) <.> "txt") 12 `shouldBe` "/pages/12.txt"
    renderRoute ("typed" <//> (var :: Var Int) <.> (var :: Var Format)) 12 Html `shouldBe` "/typed/12.html"
    renderRoute ("compressed" <//> (var :: Var Int) <.> (var :: Var Format) <.> "gz") 12 Txt `shouldBe` "/compressed/12.txt.gz"
    renderRouteEncodedWith StrictSlashes (trailingSlash $ "directory" <//> (var :: Var Int) <.> (var :: Var Format)) 12 Txt
      `shouldBe` "/directory/12.txt/"
    renderRoute ("report" <.> "txt" <//> "download") `shouldBe` "/report.txt/download"

makeApp :: SlashPolicy -> IO Wai.Application
makeApp policy = spockAsApp $ spockConfigT (defaultSpockConfig { sc_slashPolicy = policy }) id $ do
  get ("pages" <//> var <.> "txt") $ \(number :: Int) -> text $ T.pack $ show number
  get ("typed" <//> var <.> var) $ \(number :: Int) (format :: Format) -> text $ T.pack (show number) <> ":" <> T.pack (show format)
  get ("names" <//> var <.> "txt") text
  get ("archives" <//> var <.> "tar.gz") text
  get ("compressed" <//> var <.> var <.> "gz") $ \(number :: Int) (format :: Format) -> text $ T.pack (show number) <> ":" <> T.pack (show format)
  get ("values" <//> var <//> "pages" <//> var <.> "txt" <//> "details" <//> var) $
    \key (page :: Int) (detail :: Int) -> text $ key <> ":" <> T.pack (show page) <> ":" <> T.pack (show detail)
  get ("priority" <//> var <.> "txt") $ \(_ :: T.Text) -> text "fixed"
  get "priority/special.txt" $ text "static"
  get ("priority" <//> var <.> var) $ \(_ :: T.Text) (_ :: T.Text) -> text "extension"
  get ("priority" <//> var) $ \(_ :: T.Text) -> text "plain"
  get (("rest" <//> var <.> "txt") <//> wildcard) $ \(number :: Int) rest -> text $ T.pack (show number) <> ":" <> rest
  get ("next" <//> var <.> "txt") $ \(number :: Int) -> text $ T.pack $ show number
  get ("next" <//> var <.> "txt") $ \(_ :: Int) -> jumpNext
  get (trailingSlash $ "directory" <//> var <.> var) $ \(number :: Int) (format :: Format) -> text $ T.pack (show number) <> ":" <> T.pack (show format)
