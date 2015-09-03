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

import qualified Data.ByteString.Char8   as BS
import           Data.Monoid             ((<>))
import           Data.String.Conversions (cs)
import qualified Data.Text               as T
import           Data.Time
import qualified Network.HTTP.Types.URI  as URI (urlEncode, urlDecode)
#if MIN_VERSION_time(1,5,0)
#else
import           System.Locale           (defaultTimeLocale)
#endif

data CookieSettings = CookieSettings { cs_EOL      :: CookieEOL
                                     , cs_path     :: T.Text
                                     , cs_domain   :: T.Text
                                     , cs_HTTPOnly :: Bool
                                     , cs_secure   :: Bool
                                     }

data CookieEOL = CookieValidUntil UTCTime
               | CookieValidFor NominalDiffTime
               | CookieValidForSession

defaultCookieSettings :: CookieSettings
defaultCookieSettings = CookieSettings { cs_EOL      = CookieValidForSession
                                       , cs_HTTPOnly = False
                                       , cs_secure   = False
                                       , cs_domain   = T.empty
                                       , cs_path     = "/"
                                       }

generateCookieHeaderString :: T.Text -> T.Text -> CookieSettings -> UTCTime -> T.Text
generateCookieHeaderString name value CookieSettings{..} now =
    T.intercalate "; " $ filter (not . T.null) [ nv
                                               , domain
                                               , path
                                               , maxAge
                                               , expires
                                               , httpOnly
                                               , secure
                                               ]
  where
      nv       = T.concat [name, "=", urlEncode value]
      path     = T.concat ["path=", cs_path]
      domain   = if T.null cs_domain then T.empty else T.concat ["domain=", cs_domain]
      httpOnly = if cs_HTTPOnly then "HttpOnly" else T.empty
      secure   = if cs_secure then "Secure" else T.empty

      maxAge = case cs_EOL of
          CookieValidForSession -> T.empty
          CookieValidFor n      -> "max-age=" <> maxAgeValue n
          CookieValidUntil t    -> "max-age=" <> maxAgeValue (diffUTCTime t now)

      expires = case cs_EOL of
          CookieValidForSession -> T.empty
          CookieValidFor n      -> "expires=" <> expiresValue (addUTCTime n now)
          CookieValidUntil t    -> "expires=" <> expiresValue t

      maxAgeValue :: NominalDiffTime -> T.Text
      maxAgeValue nrOfSeconds =
          let v = round (max nrOfSeconds 0) :: Integer
          in  T.pack (show v)

      expiresValue :: UTCTime -> T.Text
      expiresValue t =
          T.pack $ formatTime defaultTimeLocale "%a, %d %b %Y %X %Z" t

      urlEncode :: T.Text -> T.Text
      urlEncode = cs . URI.urlEncode True . cs

parseCookies :: BS.ByteString -> [(T.Text, T.Text)]
parseCookies = map parseCookie . BS.split ';'
  where
    parseCookie :: BS.ByteString -> (T.Text, T.Text)
    parseCookie cstr =
        let (name, urlEncValue) = BS.break (== '=') cstr
        in  (cs name, cs . URI.urlDecode True . BS.drop 1 $ urlEncValue)

