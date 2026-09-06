{-# LANGUAGE OverloadedStrings #-}

module Web.Spock.Internal.SessionCommon (createSessionAt, randomHash) where

import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import qualified System.Entropy as Entropy
import Web.Spock.Internal.Types

createSessionAt :: SessionCfg conn sess st -> UTCTime -> sess -> IO (Session conn sess st)
createSessionAt cfg now content = do
  sid <- randomHash (sc_sessionIdEntropy cfg)
  csrfToken <- randomHash 12
  pure $ Session sid csrfToken (addUTCTime (sc_sessionTTL cfg) now) content

randomHash :: Int -> IO T.Text
randomHash len = do
  bytes <- Entropy.getEntropy len
  pure $ T.replace "=" "" $ T.replace "/" "_" $ T.replace "+" "-" $ T.decodeUtf8 $ B64.encode bytes
