{-# LANGUAGE OverloadedStrings #-}
module Web.Spock.Cookie where

import Web.Spock.Types

import Control.Arrow
import Control.Monad.Trans
import Data.Time
import System.Locale
import Web.Scotty.Trans
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Network.Wai as Wai

-- | Read a cookie previously set in the users browser for your site
getCookie :: (SpockError e, MonadIO m) => T.Text -> ActionT e m (Maybe T.Text)
getCookie name =
     do r <- request
        return $ getCookieFromReq name r

getCookieFromReq :: T.Text -> Wai.Request -> Maybe T.Text
getCookieFromReq name req =
    lookup "cookie" (Wai.requestHeaders req) >>=
           lookup name . parseCookies . T.decodeUtf8
    where
      parseCookies :: T.Text -> [(T.Text, T.Text)]
      parseCookies = map parseCookie . T.splitOn ";" . T.concat . T.words
      parseCookie = first T.init . T.breakOnEnd "="

-- | Set a cookie living for a given number of seconds
setCookie :: (SpockError e, MonadIO m) => T.Text -> T.Text -> NominalDiffTime
          -> ActionT e m ()
setCookie name value validSeconds =
    do now <- liftIO getCurrentTime
       setCookie' name value (validSeconds `addUTCTime` now)

-- | Set a cookie living until a specific 'UTCTime'
setCookie' :: (SpockError e, MonadIO m) => T.Text -> T.Text -> UTCTime
           -> ActionT e m ()
setCookie' name value validUntil =
    setHeader "Set-Cookie" (renderCookie name value validUntil)

renderCookie :: T.Text -> T.Text -> UTCTime -> TL.Text
renderCookie name value validUntil =
    let formattedTime =
            TL.pack $ formatTime defaultTimeLocale "%a, %d-%b-%Y %X %Z" validUntil
    in TL.concat [ TL.fromStrict name
                 , "="
                 , TL.fromStrict value
                 , "; path=/; expires="
                 , formattedTime
                 , ";"
                 ]
