{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Web.Spock.Internal.Cookies
    ( CookieSettings(..)
    , defaultCookieSettings
    , CookieEOL(..)
    , generateCookieHeaderString
    , parseCookies
    )
where

import Data.Maybe
import Data.Monoid ((<>))
import Data.Time
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types.URI as URI (urlEncode, urlDecode)
#if MIN_VERSION_time(1,5,0)
#else
import System.Locale (defaultTimeLocale)
#endif

-- | Cookie settings
data CookieSettings
   = CookieSettings
   { cs_EOL :: CookieEOL
     -- ^ cookie expiration setting, see 'CookieEOL'
   , cs_path :: Maybe BS.ByteString
     -- ^ a path for the cookie
   , cs_domain :: Maybe BS.ByteString
     -- ^ a domain for the cookie. 'Nothing' means no domain is set
   , cs_HTTPOnly :: Bool
     -- ^ whether the cookie should be set as HttpOnly
   , cs_secure :: Bool
     -- ^ whether the cookie should be marked secure (sent over HTTPS only)
   }

-- | Setting cookie expiration
data CookieEOL
   = CookieValidUntil UTCTime
   -- ^ a point in time in UTC until the cookie is valid
   | CookieValidFor NominalDiffTime
   -- ^ a period (in seconds) for which the cookie is valid
   | CookieValidForSession
   -- ^ the cookie expires with the browser session

-- | Default cookie settings, equals
--
-- > CookieSettings
-- >   { cs_EOL      = CookieValidForSession
-- >   , cs_HTTPOnly = False
-- >   , cs_secure   = False
-- >   , cs_domain   = Nothing
-- >   , cs_path     = Just "/"
-- >   }
--
defaultCookieSettings :: CookieSettings
defaultCookieSettings =
    CookieSettings
    { cs_EOL      = CookieValidForSession
    , cs_HTTPOnly = False
    , cs_secure   = False
    , cs_domain   = Nothing
    , cs_path     = Just "/"
    }

generateCookieHeaderString ::
    T.Text
    -> T.Text
    -> CookieSettings
    -> UTCTime
    -> BS.ByteString
generateCookieHeaderString name value CookieSettings{..} now =
    BS.intercalate "; " fullCookie
    where
      fullCookie =
          catMaybes
          [ nv
          , domain
          , path
          , maxAge
          , expires
          , httpOnly
          , secure
          ]
      nv = Just $ BS.concat [T.encodeUtf8 name, "=", urlEncode value]
      path = flip fmap cs_path $ \p -> BS.concat ["path=", p]
      domain =
          flip fmap cs_domain $ \d ->
          BS.concat ["domain=", d]
      httpOnly = if cs_HTTPOnly then Just "HttpOnly" else Nothing
      secure = if cs_secure then Just "Secure" else Nothing

      maxAge =
          case cs_EOL of
            CookieValidForSession -> Nothing
            CookieValidFor n -> Just $ "max-age=" <> maxAgeValue n
            CookieValidUntil t -> Just $ "max-age=" <> maxAgeValue (diffUTCTime t now)

      expires =
          case cs_EOL of
            CookieValidForSession -> Nothing
            CookieValidFor n -> Just $ "expires=" <> expiresValue (addUTCTime n now)
            CookieValidUntil t -> Just $ "expires=" <> expiresValue t

      maxAgeValue :: NominalDiffTime -> BS.ByteString
      maxAgeValue nrOfSeconds =
          let v = round (max nrOfSeconds 0) :: Integer
          in  BS.pack (show v)

      expiresValue :: UTCTime -> BS.ByteString
      expiresValue t =
          BS.pack $ formatTime defaultTimeLocale "%a, %d-%b-%Y %X %Z" t

      urlEncode :: T.Text -> BS.ByteString
      urlEncode = URI.urlEncode True . T.encodeUtf8

parseCookies :: BS.ByteString -> [(T.Text, T.Text)]
parseCookies =
    map parseCookie . BS.split ';'
    where
      parseCookie :: BS.ByteString -> (T.Text, T.Text)
      parseCookie cstr =
          let (name, urlEncValue) = BS.break (== '=') cstr
          in  (T.strip $ T.decodeUtf8 name, T.decodeUtf8 . URI.urlDecode True . BS.drop 1 $ urlEncValue)
