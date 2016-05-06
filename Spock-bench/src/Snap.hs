{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Snap where

import Snap.Http.Server
import Snap.Http.Server.Config
import Snap.Core
import Data.String
import Control.Monad
import qualified Data.Text.Lazy as TL

runApp :: Int -> IO ()
runApp port =
    simpleHttpServe (setPort port mempty) $
    route
    ( ( "hello"
      , writeText "Hello world"
      )
    : ( "echo/:param"
      , do (t :: Int) <- fromJust <$> getParam "param"
           writeLazyText (TL.pack $ show t)
      )
    : map mkRoute complexDeep
    )
    where
      complexDeep :: [(Int, Int, Int)]
      complexDeep =
          [(x, y, z) | x <- [0..10], y <- [0..10], z <- [0..10]]
{-
    scotty port $
    do get "hello" $ text "Hello world"
       get "plus/:param" $
          do (t :: Int) <- param "param"
             text (TL.pack $ show (t + 1))
       forM_ complexDeep $ \(a, b, c) ->
           get (fromString $ "deep/" ++ show a ++ "/" ++ show b ++ "/" ++ show c) $
           text "Found me!"
    where
        complexDeep :: [(Int, Int, Int)]
        complexDeep =
            [(x, y, z) | x <- [0..10], y <- [0..10], z <- [0..10]]
-}
