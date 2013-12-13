{-# LANGUAGE OverloadedStrings #-}
module Web.Spock.Cookie where

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
getCookie :: MonadIO m => T.Text -> ActionT m (Maybe T.Text)
getCookie name =
    do req <- request
       return $ lookup "cookie" (Wai.requestHeaders req) >>=
              lookup name . parseCookies . T.decodeUtf8
    where
      parseCookies :: T.Text -> [(T.Text, T.Text)]
      parseCookies = map parseCookie . T.splitOn ";" . T.concat . T.words
      parseCookie = first T.init . T.breakOnEnd "="

-- | Set a cookie living for a given number of seconds
setCookie :: MonadIO m => T.Text -> T.Text -> NominalDiffTime -> ActionT m ()
setCookie name value validSeconds =
    do now <- liftIO getCurrentTime
       setCookie' name value (validSeconds `addUTCTime` now)

-- | Set a cookie living until a specific 'UTCTime'
setCookie' :: MonadIO m => T.Text -> T.Text -> UTCTime -> ActionT m ()
setCookie' name value validUntil =
    let formattedTime =
            TL.pack $ formatTime defaultTimeLocale "%a, %d-%b-%Y %X %Z" validUntil
    in setHeader "Set-Cookie" (TL.concat [ TL.fromStrict name
                                         , "="
                                         , TL.fromStrict value
                                         , "; path=/; expires="
                                         , formattedTime
                                         , ";"
                                         ]
                              )
