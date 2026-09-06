{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai
import People (withPeopleApp)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

main :: IO ()
main = hspec $ do
  around withDatabase $ describe "Persistent REST tutorial" $ do
    it "creates a person with 201, a Location header, and a retrievable ID" $ \app -> do
      created <- send app "POST" "/people" validPerson
      Wai.simpleStatus created `shouldBe` status201
      lookup "Location" (Wai.simpleHeaders created) `shouldBe` Just "/people/1"
      decode (Wai.simpleBody created) `shouldBe` Just (object ["result" .= String "success", "id" .= (1 :: Int)])
      found <- send app "GET" "/people/1" ""
      Wai.simpleStatus found `shouldBe` status200
      decode (Wai.simpleBody found) `shouldBe` Just personOne
      listed <- send app "GET" "/people" ""
      decode (Wai.simpleBody listed) `shouldBe` Just [personOne]
      lookup "Set-Cookie" (Wai.simpleHeaders listed) `shouldBe` Nothing
    it "rejects malformed JSON and invalid field values without inserting them" $ \app -> do
      forM_ ["bad-json", "{}", "{\"name\":\"Alex\",\"age\":-1}", "{\"name\":\" \",\"age\":25}",
             "{\"name\":\"Alex\",\"age\":131}"] $ \payload -> do
        res <- send app "POST" "/people" payload
        assertJsonStatus status400 res
      listed <- send app "GET" "/people" ""
      decode (Wai.simpleBody listed) `shouldBe` Just ([] :: [Value])
    it "uses JSON 404 responses for absent people, invalid captures, and unmatched routes" $ \app ->
      forM_ ["/people/42", "/people/not-an-id", "/missing"] $ \path ->
        send app "GET" path "" >>= assertJsonStatus status404
    it "updates existing people with 200 and deletes them with an empty 204" $ \app -> do
      _ <- send app "POST" "/people" validPerson
      updated <- send app "PUT" "/people/1" "{\"name\":\"Ada\",\"age\":30}"
      Wai.simpleStatus updated `shouldBe` status200
      decode (Wai.simpleBody updated) `shouldBe` Just (object ["id" .= (1 :: Int), "name" .= String "Ada", "age" .= (30 :: Int)])
      deleted <- send app "DELETE" "/people/1" ""
      Wai.simpleStatus deleted `shouldBe` status204
      Wai.simpleBody deleted `shouldBe` ""
      send app "GET" "/people/1" "" >>= assertJsonStatus status404
    it "returns 404 when updating or deleting a missing person" $ \app -> do
      send app "PUT" "/people/42" validPerson >>= assertJsonStatus status404
      send app "DELETE" "/people/42" "" >>= assertJsonStatus status404
    it "returns JSON 413 when a consumed body exceeds the limit" $ \app ->
      send app "POST" "/people" (BL.replicate (16 * 1024 + 1) 120) >>= assertJsonStatus status413
  it "retains data after closing the pool and reopening the same SQLite file" $
    withSystemTempDirectory "spock-rest-persistence" $ \directory -> do
      let database = T.pack $ directory </> "people.sqlite"
      withPeopleApp database $ \app -> do
        res <- send app "POST" "/people" validPerson
        Wai.simpleStatus res `shouldBe` status201
      withPeopleApp database $ \app -> do
        res <- send app "GET" "/people/1" ""
        decode (Wai.simpleBody res) `shouldBe` Just personOne

validPerson :: BL.ByteString
validPerson = "{\"name\":\"Alex\",\"age\":25}"

personOne :: Value
personOne = object ["id" .= (1 :: Int), "name" .= String "Alex", "age" .= (25 :: Int)]

withDatabase :: (Wai.Application -> IO a) -> IO a
withDatabase use = withSystemTempDirectory "spock-rest" $ \directory ->
  withPeopleApp (T.pack $ directory </> "people.sqlite") use

assertJsonStatus :: Status -> Wai.SResponse -> Expectation
assertJsonStatus status res = do
  Wai.simpleStatus res `shouldBe` status
  lookup "Content-Type" (Wai.simpleHeaders res) `shouldSatisfy` maybe False ("application/json" `BS.isPrefixOf`)
  decode (Wai.simpleBody res) `shouldSatisfy` maybe False isObject
  where
    isObject (Object _) = True
    isObject _ = False

send :: Wai.Application -> Method -> BS.ByteString -> BL.ByteString -> IO Wai.SResponse
send app method path payload = Wai.runSession
  (Wai.srequest $ Wai.SRequest ((Wai.setPath Wai.defaultRequest path)
    { Wai.requestMethod = method, Wai.requestHeaders = [("Content-Type", "application/json")] }) payload) app
