{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Web.Spock.SafeSpec (spec) where

import Web.Spock

import Control.Arrow (second)
import Control.Monad
import Data.IORef
import Data.List (find)
import Test.Hspec
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.Wai.Test as Wai
import qualified Test.Hspec.Wai as Test

sessionSpec :: Spec
sessionSpec =
    describe "SessionManager" $
    Test.with sessionApp $
    do it "should generate random session ids" $
          do ids <- Test.liftIO $ newIORef HS.empty
             -- of course this sample is too small to prove anything, but it's a good
             -- smoke test...
             replicateM_ 500 $
                do res <- Test.get "/test"
                   Test.liftIO $ checkCookie ids res `shouldReturn` True
       it "should remember a session" $
          do res <- Test.get "/set/5"
             case getSessCookie res of
               Nothing ->
                   Test.liftIO $ expectationFailure "Missing spockcookie"
               Just sessCookie ->
                   Test.request "GET" "/check" [("Cookie", T.encodeUtf8 sessCookie)] ""
                           `Test.shouldRespondWith` "5"
       it "should regenerate and preserve all content" $
          do res <- Test.get "/set/5"
             case getSessCookie res of
               Nothing ->
                   Test.liftIO $ expectationFailure "Missing spockcookie"
               Just sessCookie ->
                   do res2 <- Test.request "GET" "/regenerate" [("Cookie", T.encodeUtf8 sessCookie)] ""
                      case getSessCookie res2 of
                        Nothing ->
                            Test.liftIO $ expectationFailure "Missing new spockcookie"
                        Just sessCookie2 ->
                            do Test.request "GET" "/check" [("Cookie", T.encodeUtf8 sessCookie2)] ""
                                       `Test.shouldRespondWith` "5"
                               Test.request "GET" "/check" [("Cookie", T.encodeUtf8 sessCookie)] ""
                                       `Test.shouldRespondWith` "0"
    where
      sessionApp =
          spockAsApp $
          spockCfg >>= \cfg ->
          spock cfg $
          do get "test" $ text "text"
             get ("set" <//> var) $ \number -> writeSession number >> text "done"
             get "regenerate" $ sessionRegenerateId >> text "done"
             get "check" $
                 do val <- readSession
                    text (T.pack $ show val)
      spockCfg =
          defaultSpockCfg (0 :: Int) PCNoDatabase True

      getSessCookie :: Wai.SResponse -> Maybe T.Text
      getSessCookie resp =
          let headers = Wai.simpleHeaders resp
          in fmap snd $ find (\h -> fst h == "Set-Cookie" && "spockcookie" `T.isPrefixOf` snd h) $
             map (second T.decodeUtf8) headers

      checkCookie :: IORef (HS.HashSet T.Text) -> Wai.SResponse -> IO Bool
      checkCookie setRef resp =
          do let mSessCookie = getSessCookie resp
             case mSessCookie of
               Nothing ->
                   do expectationFailure "Missing spockcookie"
                      return False
               Just sessCookie ->
                   let (_, cval) = T.breakOn "=" $ fst $ T.breakOn ";" sessCookie
                   in do set <- readIORef setRef
                         if HS.member cval set
                         then return False
                         else do writeIORef setRef $ HS.insert cval set
                                 return True

spec :: Spec
spec =
    describe "SafeRouting" $ sessionSpec
