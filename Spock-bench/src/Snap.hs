{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Snap where

import Shared

import Snap.Http.Server
import Snap.Core
import Data.Maybe
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text.Lazy as TL

runApp :: Int -> IO ()
runApp port =
    httpServe (setErrorLog ConfigNoLog $ setAccessLog ConfigNoLog $ setPort port mempty) $
    route
    ( ( "hello"
      , writeText "Hello world"
      )
    : ( "plus/:param"
      , do t <- fromJust <$> getParam "param"
           let i :: Int
               i = read (BSC.unpack t)
           writeLazyText (TL.pack $ show (i + 1))
      )
    : map mkRoute complexDeep
    )
    where
      mkRoute (a, b, c) =
          ( BSC.pack $ "deep/" ++ show a ++ "/" ++ show b ++ "/" ++ show c
          , writeText "Found me!"
          )
