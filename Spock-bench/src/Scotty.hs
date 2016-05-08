{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Scotty where

import Shared

import Web.Scotty
import Data.String
import Control.Monad
import qualified Data.Text.Lazy as TL

runApp :: Int -> IO ()
runApp port =
    scotty port $
    do get "/hello" $ text "Hello world"
       get "/plus/:param" $
          do (t :: Int) <- param "param"
             text (TL.pack $ show (t + 1))
       forM_ complexDeep $ \(a, b, c) ->
           get (fromString $ "/deep/" ++ show a ++ "/" ++ show b ++ "/" ++ show c) $
           text "Found me!"
