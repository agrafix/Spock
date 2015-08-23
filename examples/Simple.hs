{-# LANGUAGE OverloadedStrings #-}
import Web.Spock.Simple

import qualified Data.Text as T

main =
    runSpock 3000 $ spockT id $
    do get ("echo" <//> ":something") $
        do Just something <- param "something"
           text $ T.concat ["Echo: ", something]
       get ("regex" <//> "{number:^[0-9]+$}") $
        do Just number <- param "number"
           text $ T.concat ["Just a number: ", number]
