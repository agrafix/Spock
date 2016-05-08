{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Servant (runApp) where

import Data.Proxy
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server
import System.IO
import qualified Data.Text as T

#define D(a,b,c) :<|> "deep" :> a :> b :> c :> Get '[PlainText] T.Text
#define IMPL :<|> (return "Found me!")

runApp :: Int -> IO ()
runApp port =
    do let settings =
               setPort port $
               setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
               defaultSettings
       runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve fullApi server

fullApi :: Proxy Api
fullApi = Proxy

-- generate server IMPL part using:
-- stack ghci
-- > replicateM_ (length complexDeep) (putStrLn "    IMPL")

server :: Server Api
server =
    return "Hello world"
    :<|> (\x -> return (T.pack $ show (x + 1)))
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL
    IMPL


-- generate api type D* part using:
-- stack ghci
-- > mapM_ (\(a, b, c) -> putStrLn $ "    D(\"" ++ show a ++ "\",\"" ++ show b ++ "\",\"" ++ show c ++ "\")")
--     complexDeep

type Api =
         "hello" :> Get '[PlainText] T.Text
    :<|> "plus" :> Capture "number" Int :> Get '[PlainText] T.Text
    D("0","0","0")
    D("0","0","1")
    D("0","0","2")
    D("0","0","3")
    D("0","0","4")
    D("0","0","5")
    D("0","1","0")
    D("0","1","1")
    D("0","1","2")
    D("0","1","3")
    D("0","1","4")
    D("0","1","5")
    D("0","2","0")
    D("0","2","1")
    D("0","2","2")
    D("0","2","3")
    D("0","2","4")
    D("0","2","5")
    D("0","3","0")
    D("0","3","1")
    D("0","3","2")
    D("0","3","3")
    D("0","3","4")
    D("0","3","5")
    D("0","4","0")
    D("0","4","1")
    D("0","4","2")
    D("0","4","3")
    D("0","4","4")
    D("0","4","5")
    D("0","5","0")
    D("0","5","1")
    D("0","5","2")
    D("0","5","3")
    D("0","5","4")
    D("0","5","5")
    D("1","0","0")
    D("1","0","1")
    D("1","0","2")
    D("1","0","3")
    D("1","0","4")
    D("1","0","5")
    D("1","1","0")
    D("1","1","1")
    D("1","1","2")
    D("1","1","3")
    D("1","1","4")
    D("1","1","5")
    D("1","2","0")
    D("1","2","1")
    D("1","2","2")
    D("1","2","3")
    D("1","2","4")
    D("1","2","5")
    D("1","3","0")
    D("1","3","1")
    D("1","3","2")
    D("1","3","3")
    D("1","3","4")
    D("1","3","5")
    D("1","4","0")
    D("1","4","1")
    D("1","4","2")
    D("1","4","3")
    D("1","4","4")
    D("1","4","5")
    D("1","5","0")
    D("1","5","1")
    D("1","5","2")
    D("1","5","3")
    D("1","5","4")
    D("1","5","5")
    D("2","0","0")
    D("2","0","1")
    D("2","0","2")
    D("2","0","3")
    D("2","0","4")
    D("2","0","5")
    D("2","1","0")
    D("2","1","1")
    D("2","1","2")
    D("2","1","3")
    D("2","1","4")
    D("2","1","5")
    D("2","2","0")
    D("2","2","1")
    D("2","2","2")
    D("2","2","3")
    D("2","2","4")
    D("2","2","5")
    D("2","3","0")
    D("2","3","1")
    D("2","3","2")
    D("2","3","3")
    D("2","3","4")
    D("2","3","5")
    D("2","4","0")
    D("2","4","1")
    D("2","4","2")
    D("2","4","3")
    D("2","4","4")
    D("2","4","5")
    D("2","5","0")
    D("2","5","1")
    D("2","5","2")
    D("2","5","3")
    D("2","5","4")
    D("2","5","5")
    D("3","0","0")
    D("3","0","1")
    D("3","0","2")
    D("3","0","3")
    D("3","0","4")
    D("3","0","5")
    D("3","1","0")
    D("3","1","1")
    D("3","1","2")
    D("3","1","3")
    D("3","1","4")
    D("3","1","5")
    D("3","2","0")
    D("3","2","1")
    D("3","2","2")
    D("3","2","3")
    D("3","2","4")
    D("3","2","5")
    D("3","3","0")
    D("3","3","1")
    D("3","3","2")
    D("3","3","3")
    D("3","3","4")
    D("3","3","5")
    D("3","4","0")
    D("3","4","1")
    D("3","4","2")
    D("3","4","3")
    D("3","4","4")
    D("3","4","5")
    D("3","5","0")
    D("3","5","1")
    D("3","5","2")
    D("3","5","3")
    D("3","5","4")
    D("3","5","5")
    D("4","0","0")
    D("4","0","1")
    D("4","0","2")
    D("4","0","3")
    D("4","0","4")
    D("4","0","5")
    D("4","1","0")
    D("4","1","1")
    D("4","1","2")
    D("4","1","3")
    D("4","1","4")
    D("4","1","5")
    D("4","2","0")
    D("4","2","1")
    D("4","2","2")
    D("4","2","3")
    D("4","2","4")
    D("4","2","5")
    D("4","3","0")
    D("4","3","1")
    D("4","3","2")
    D("4","3","3")
    D("4","3","4")
    D("4","3","5")
    D("4","4","0")
    D("4","4","1")
    D("4","4","2")
    D("4","4","3")
    D("4","4","4")
    D("4","4","5")
    D("4","5","0")
    D("4","5","1")
    D("4","5","2")
    D("4","5","3")
    D("4","5","4")
    D("4","5","5")
    D("5","0","0")
    D("5","0","1")
    D("5","0","2")
    D("5","0","3")
    D("5","0","4")
    D("5","0","5")
    D("5","1","0")
    D("5","1","1")
    D("5","1","2")
    D("5","1","3")
    D("5","1","4")
    D("5","1","5")
    D("5","2","0")
    D("5","2","1")
    D("5","2","2")
    D("5","2","3")
    D("5","2","4")
    D("5","2","5")
    D("5","3","0")
    D("5","3","1")
    D("5","3","2")
    D("5","3","3")
    D("5","3","4")
    D("5","3","5")
    D("5","4","0")
    D("5","4","1")
    D("5","4","2")
    D("5","4","3")
    D("5","4","4")
    D("5","4","5")
    D("5","5","0")
    D("5","5","1")
    D("5","5","2")
    D("5","5","3")
    D("5","5","4")
    D("5","5","5")
