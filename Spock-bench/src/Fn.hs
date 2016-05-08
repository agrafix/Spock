{-# LANGUAGE OverloadedStrings #-}
module Fn (runApp) where

import Shared

import Network.Wai (Response)
import Network.Wai.Handler.Warp (run)
import Web.Fn
import qualified Data.Text as T

data Ctxt = Ctxt { _req :: FnRequest }
instance RequestContext Ctxt where
  getRequest = _req
  setRequest c r = c { _req = r }

initializer :: IO Ctxt
initializer = return (Ctxt defaultFnRequest)

runApp :: Int -> IO ()
runApp port =
    do ctxt <- initializer
       run port $ toWAI ctxt site

site :: Ctxt -> IO Response
site ctxt =
    route ctxt
    ( (path "hello" ==> indexH)
    : (path "plus" // segment ==> plusH)
    : map mkRoute complexDeep
    )
    `fallthrough` notFoundText "Page not found."

indexH :: Ctxt -> IO (Maybe Response)
indexH _ = okText "Hello world"

plusH :: Ctxt -> Int -> IO (Maybe Response)
plusH _ t = okText $ T.pack (show (t + 1))

mkRoute :: (Int, Int, Int) -> Ctxt -> Req -> Maybe (IO (Maybe Response))
mkRoute (a, b, c) =
    path "deep" // path (showt a) // path (showt b) // path (showt c) ==> (\_ -> okText "Found me!")

showt :: Show a => a -> T.Text
showt = T.pack . show
