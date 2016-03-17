{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Web.Spock.Internal.SessionVaultSpec (spec) where

import Web.Spock.Internal.SessionVault

import Control.Concurrent.STM
import Test.Hspec

data DummySession
    = DummySession
      { ds_key :: Int
      , ds_value :: Int
      } deriving (Show, Eq)

instance IsSession DummySession where
    type SessionKey DummySession = Int
    getSessionKey = ds_key

spec :: Spec
spec =
     describe "SessionVault" $
     do it "insert works correctly" $
           do let sess = DummySession 1 1
              vault <- atomically newSessionVault
              atomically $ storeSession sess vault
              atomically (loadSession (ds_key sess) vault) `shouldReturn` Just sess
        it "update works correctly" $
           do let sess = DummySession 1 1
                  sess2 = DummySession 1 2
              vault <- atomically newSessionVault
              atomically $ storeSession sess vault
              atomically (loadSession (ds_key sess) vault) `shouldReturn` Just sess
              atomically $ storeSession sess2 vault
              atomically (loadSession (ds_key sess) vault) `shouldReturn` Just sess2
        it "filter and toList works correctly" $
           do let sess = DummySession 1 1
                  sess2 = DummySession 2 2
                  sess3 = DummySession 3 3
              vault <- atomically newSessionVault
              atomically $ mapM_ (`storeSession` vault) [sess, sess2, sess3]
              atomically (toList vault) `shouldReturn` [sess, sess2, sess3]
              atomically $ filterSessions (\s -> ds_value s > 2) vault
              atomically (toList vault) `shouldReturn` [sess3]
