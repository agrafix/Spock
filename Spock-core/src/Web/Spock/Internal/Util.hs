{-# LANGUAGE OverloadedStrings #-}
module Web.Spock.Internal.Util where

import Data.Maybe
import Network.HTTP.Types
import Network.Wai.Internal
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

data ClientPreferredFormat
   = PrefJSON
   | PrefXML
   | PrefHTML
   | PrefText
   | PrefUnknown
   deriving (Show, Eq)

mimeMapping :: HM.HashMap T.Text ClientPreferredFormat
mimeMapping =
    HM.fromList
    [ ("application/json", PrefJSON)
    , ("text/javascript", PrefJSON)
    , ("text/json", PrefJSON)
    , ("application/javascript", PrefJSON)
    , ("application/xml", PrefXML)
    , ("text/xml", PrefXML)
    , ("text/plain", PrefText)
    , ("text/html", PrefHTML)
    , ("application/xhtml+xml", PrefHTML)
    ]

detectPreferredFormat :: T.Text -> ClientPreferredFormat
detectPreferredFormat t =
    let (mimeTypeStr, _) = T.breakOn ";" t
        mimeTypes = map (T.toLower . T.strip) $ T.splitOn "," mimeTypeStr
        firstMatch [] = PrefUnknown
        firstMatch (x:xs) = fromMaybe (firstMatch xs) (HM.lookup x mimeMapping)
    in firstMatch mimeTypes


mapReqHeaders :: (ResponseHeaders -> ResponseHeaders) -> Response -> Response
mapReqHeaders f resp =
    case resp of
      (ResponseFile s h b1 b2) -> ResponseFile s (f h) b1 b2
      (ResponseBuilder s h b) -> ResponseBuilder s (f h) b
      (ResponseStream s h b) -> ResponseStream s (f h) b
      (ResponseRaw x r) -> ResponseRaw x (mapReqHeaders f r)
