{-# LANGUAGE OverloadedStrings #-}
module Web.Spock.TestUtils where

import Web.Spock.Internal.Cookies

import Data.List (find)
import qualified Data.Text as T
import qualified Network.Wai.Test as Wai

getSessCookie :: Wai.SResponse -> Maybe T.Text
getSessCookie resp =
    let headers = Wai.simpleHeaders resp
    in lookup "spockcookie" $
       maybe [] (parseCookies . snd) $
       find (\h -> fst h == "Set-Cookie") headers
