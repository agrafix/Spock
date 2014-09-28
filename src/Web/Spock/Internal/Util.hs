module Web.Spock.Internal.Util where

import Network.HTTP.Types
import Network.Wai.Internal

mapReqHeaders :: (ResponseHeaders -> ResponseHeaders) -> Response -> Response
mapReqHeaders f resp =
    case resp of
      (ResponseFile s h b1 b2) -> ResponseFile s (f h) b1 b2
      (ResponseBuilder s h b) -> ResponseBuilder s (f h) b
      (ResponseStream s h b) -> ResponseStream s (f h) b
      (ResponseRaw x r) -> ResponseRaw x (mapReqHeaders f r)
