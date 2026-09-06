{-# LANGUAGE OverloadedStrings #-}

-- | Encrypted, authenticated sessions stored entirely in a browser cookie.
-- Uses Crypton's XChaCha20-Poly1305 with fresh 192-bit OS-random nonces.
-- Keys are supplied by the application; this library never writes them to disk.
--
-- Use 'cookieSessionCodec' with @defaultClientSessionCfg@ and @ClientSessions@
-- from "Web.Spock.Config".
-- Spock checks the authenticated expiry using the server clock. Use HTTPS,
-- Secure/HttpOnly cookies and CSRF protection, for example defaultBrowserSpockCfg.
--
-- Stateless cookies cannot revoke individual sessions or merge simultaneous
-- requests. Logout expires this browser's cookie; copies remain replayable until
-- expiry or key removal. Use a server backend when immediate revocation or
-- concurrent counters are required. Keep payloads small; Spock limits the entire
-- Set-Cookie value to 4096 bytes and rejects oversized changes before saving them.
module Web.Spock.Session.Cookie
  ( CookieKey, CookieKeyRing, CookieKeyError (..),
    cookieKey, cookieKeyRing, cookieSessionCodec
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (guard)
import qualified Crypto.Cipher.ChaChaPoly1305 as C
import Crypto.Error (CryptoFailable (..))
import Data.Aeson
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Lazy as BL
import Data.List (nub)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Entropy (getEntropy)
import Web.Spock.Config (ClientSessionCodec (..))
import Web.Spock.Internal.SessionManager (Session (..))

-- | Validated 256-bit key and public wire identifier. No Show instance; identifiers
-- are visible in the cookie, but key material must come from a secret manager.
data CookieKey = CookieKey T.Text BA.ScrubbedBytes

-- | Application namespace, primary encryption key, and accepted older keys.
-- Deploy a new primary with the old key still accepted, then remove the old key
-- after its last possible session expiry. All workers must share the key ring.
data CookieKeyRing = CookieKeyRing T.Text CookieKey [CookieKey]

-- | Configuration errors never include key material or cookie contents.
data CookieKeyError = InvalidKeyId | InvalidKeyLength | InvalidNamespace
  | DuplicateKeyId | TooManyKeys | CookieEncryptionFailure
  deriving (Eq, Show)

instance Exception CookieKeyError

-- | A 32-byte key and a 1–32 character identifier using ASCII letters, digits,
-- underscore or hyphen. Generate keys with a cryptographic OS random source.
cookieKey :: T.Text -> BS.ByteString -> Either CookieKeyError CookieKey
cookieKey name bytes
  | T.null name || T.length name > 32 || not (T.all valid name) = Left InvalidKeyId
  | BS.length bytes /= 32 = Left InvalidKeyLength
  | otherwise = Right $ CookieKey name (BA.convert bytes)
  where
    valid c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'
      || c >= '0' && c <= '9' || c == '_' || c == '-'

-- | Bind cookies to an application namespace (1–128 UTF-8 bytes). Accept at
-- most seven old keys, with unique identifiers across the entire ring.
cookieKeyRing :: T.Text -> CookieKey -> [CookieKey] -> Either CookieKeyError CookieKeyRing
cookieKeyRing namespace primary old
  | T.null namespace || BS.length (T.encodeUtf8 namespace) > 128 = Left InvalidNamespace
  | length old > 7 = Left TooManyKeys
  | length ids /= length (nub ids) = Left DuplicateKeyId
  | otherwise = Right $ CookieKeyRing namespace primary old
  where ids = [name | CookieKey name _ <- primary : old]

-- | A versioned JSON codec. All session fields are encrypted and authenticated;
-- the namespace, cookie name, wire version and key identifier are authenticated
-- as associated data. Decoding never releases unauthenticated JSON. An accepted
-- old key triggers reissue with the primary key, including with fixed expiry.
--
-- Changing the namespace or removing a key invalidates affected cookies. The
-- decoder also bounds input size before attempting cryptography. It returns
-- Nothing for malformed, tampered or unknown-key input; the manager creates a
-- new empty session only if the request uses one.
cookieSessionCodec :: (ToJSON sess, FromJSON sess) => CookieKeyRing -> ClientSessionCodec sess
cookieSessionCodec (CookieKeyRing namespace primary old) = ClientSessionCodec encodeSession decodeSession
  where
    CookieKey primaryId primaryBytes = primary
    aad name kid = BL.toStrict $ encode (["Spock.session.cookie", "v1", namespace, name, kid] :: [T.Text])
    encodeSession name session = do
      nonce <- getEntropy 24
      state <- case C.nonce24 nonce >>= C.initializeX primaryBytes of
        CryptoPassed s -> pure $ C.finalizeAAD $ C.appendAAD (aad name primaryId) s
        CryptoFailed _ -> throwIO CookieEncryptionFailure
      let plain = BL.toStrict $ encode $ object
            ["id" .= sess_id session, "csrf" .= sess_csrfToken session,
             "expires" .= sess_validUntil session, "data" .= sess_data session]
          (encrypted, finalState) = C.encrypt plain state
          packed = nonce <> encrypted <> (BA.convert (C.finalize finalState) :: BS.ByteString)
      pure $ "v1." <> T.encodeUtf8 primaryId <> "." <> B64.encodeUnpadded packed
    decodeSession name input = pure $ do
      guard (BS.length input <= 4096)
      (kid, encoded) <- case BS.split 46 input of
        ["v1", k, value] -> Just (k, value)
        _ -> Nothing
      key <- lookup kid [(T.encodeUtf8 k, bytes) | CookieKey k bytes <- primary : old]
      packed <- either (const Nothing) Just $ B64.decodeUnpadded encoded
      -- Reject alternate base64 spellings of the same authenticated message.
      guard (B64.encodeUnpadded packed == encoded && BS.length packed >= 40)
      let (nonce, rest) = BS.splitAt 24 packed
          (encrypted, tag) = BS.splitAt (BS.length rest - 16) rest
      initial <- case C.nonce24 nonce >>= C.initializeX key of
        CryptoPassed s -> Just s
        CryptoFailed _ -> Nothing
      kidText <- either (const Nothing) Just $ T.decodeUtf8' kid
      let state = C.finalizeAAD $ C.appendAAD (aad name kidText) initial
          (plain, finalState) = C.decrypt encrypted state
          expected = BA.convert (C.finalize finalState) :: BS.ByteString
      guard (BA.constEq expected tag)
      value <- decodeStrict' plain
      session <- parseMaybeSession value
      pure (session, kid /= T.encodeUtf8 primaryId)

parseMaybeSession :: FromJSON sess => Value -> Maybe (Session conn sess st)
parseMaybeSession value = case fromJSON value of
  Success (SessionPayload session) -> Just session
  Error _ -> Nothing

newtype SessionPayload conn sess st = SessionPayload (Session conn sess st)

instance FromJSON sess => FromJSON (SessionPayload conn sess st) where
  parseJSON = withObject "Session" $ \o -> SessionPayload <$> (Session
    <$> o .: "id" <*> o .: "csrf" <*> o .: "expires" <*> o .: "data")
