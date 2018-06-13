{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE CPP #-}
module Web.Spock.SafeSpec (spec) where

import Web.Spock.TestUtils

import Web.Spock
import Web.Spock.Config

import Control.Monad
import Data.IORef
#if MIN_VERSION_base(4,11,0)
#else
import Data.Monoid
#endif
import Test.Hspec
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.Wai.Test as Wai
import qualified Test.Hspec.Wai as Test
import qualified Test.Hspec.Wai.Internal as Test

sessionSpec :: Spec
sessionSpec =
    describe "SessionManager" $
    do Test.with sessionApp $
          it "should generate random session ids" $
          do ids <- Test.liftIO $ newIORef HS.empty
             -- of course this sample is too small to prove anything, but it's a good
             -- smoke test...
             replicateM_ 500 $
                do Test.WaiSession (Wai.deleteClientCookie "spockcookie")
                   res <- Test.get "/test"
                   Test.liftIO $ checkCookie ids res `shouldReturn` True
       Test.with sessionApp $
          it "should remember a session" $
          do res <- Test.get "/set/5"
             case getSessCookie res of
               Nothing ->
                   Test.liftIO $ expectationFailure "Missing spockcookie"
               Just sessCookie ->
                   Test.request "GET" "/check" [("Cookie", T.encodeUtf8 $ "spockcookie=" <> sessCookie)] ""
                           `Test.shouldRespondWith` "5"
       Test.with sessionApp $
           it "should update internal session id correctly" $
           do bdy <- fmap Wai.simpleBody (Test.get "/regenerate-different-sids")
              case BSLC.split '|' bdy of
                [a, b] | a /= b -> pure ()
                xs -> Test.liftIO $ expectationFailure ("Bad result: " ++ show xs)
       Test.with sessionApp $
          it "should regenerate and preserve all content" $
          do res <- Test.get "/set/5"
             case getSessCookie res of
               Nothing ->
                   Test.liftIO $ expectationFailure "Missing spockcookie"
               Just sessCookie ->
                   do res2 <- Test.request "GET" "/regenerate" [("Cookie", T.encodeUtf8 $ "spockcookie=" <> sessCookie)] ""
                      case getSessCookie res2 of
                        Nothing ->
                            Test.liftIO $ expectationFailure "Missing new spockcookie"
                        Just sessCookie2 ->
                            do Test.request "GET" "/check" [("Cookie", T.encodeUtf8 $ "spockcookie=" <> sessCookie2)] ""
                                       `Test.shouldRespondWith` "5"
                               Test.request "GET" "/check" [("Cookie", T.encodeUtf8 $ "spockcookie=" <> sessCookie)] ""
                                       `Test.shouldRespondWith` "0"
    where
      sessionApp =
          spockAsApp $
          spockCfg >>= \cfg ->
          spock cfg $
          do get "test" $ getSessionId >>= text
             get ("set" <//> var) $ \number -> writeSession number >> text "done"
             get "regenerate" $ sessionRegenerateId >> text "done"
             get "regenerate-different-sids" $
                 do s1 <- getSessionId
                    sessionRegenerateId
                    s2 <- getSessionId
                    text $ s1 <> "|" <> s2
             get "check" $
                 do val <- readSession
                    text (T.pack $ show val)
      spockCfg =
          defaultSpockCfg (0 :: Int) PCNoDatabase True

      checkCookie :: IORef (HS.HashSet T.Text) -> Wai.SResponse -> IO Bool
      checkCookie setRef resp =
          do let mSessCookie = getSessCookie resp
             case mSessCookie of
               Nothing ->
                   do expectationFailure "Missing spockcookie"
                      return False
               Just sessionId ->
                   do set <- readIORef setRef
                      if HS.member sessionId set
                          then fail $ "Reused " ++ show sessionId ++ " (" ++ show set ++ ")"
                          else do writeIORef setRef $ HS.insert sessionId set
                                  return True

spec :: Spec
spec =
    describe "SafeRouting" $ sessionSpec
