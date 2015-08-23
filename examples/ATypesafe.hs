{-# LANGUAGE OverloadedStrings #-}
import Web.Spock

import qualified Data.Text as T

main =
    runSpock 3000 $ spockT id $
    do get ("echo" <//> var) $ \something ->
        text $ T.concat ["Echo: ", something]
