{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock.Safe

main :: IO ()
main =
    runSpock 8080 $ spockT id $
    do get "foo" $
           text "bar"
       get ("hello" <//> var) $ \name ->
           text name
