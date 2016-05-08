{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Spock where

import Shared

import Web.Spock
import Control.Monad
import qualified Data.Text as T

runApp :: Int -> IO ()
runApp port =
    runSpock port $ spockT id $
    do get "hello" $ text "Hello world"
       get ("plus" <//> var) $ \(t :: Int) -> text (T.pack $ show (t + 1))
       forM_ complexDeep $ \(a, b, c) ->
           get ("deep" <//> static (show a)
                <//> static (show b)
                <//> static (show c)) $ text "Found me!"
