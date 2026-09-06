{-# LANGUAGE OverloadedStrings #-}

module Web.Spock.TestUtils where

import Web.Spock.Internal.SessionManager
import Data.List (find)
import qualified Data.Text as T
import qualified Network.Wai.Test as Wai
import Web.Spock.Internal.Cookies

getSessCookie :: Wai.SResponse -> Maybe T.Text
getSessCookie resp =
  let headers = Wai.simpleHeaders resp
   in lookup "spockcookie" $
        maybe [] (parseCookies . snd) $
          find (\h -> fst h == "Set-Cookie") headers

serverCapability :: SessionManager IO conn sess st -> IO (ServerSessionManager IO sess)
serverCapability = maybe (fail "Expected server-session capability") pure . sm_serverSessions
