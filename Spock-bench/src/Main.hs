{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Spock
import qualified Scotty
import qualified Snap
import qualified Fn
import qualified Servant

import Control.Monad
import Network.Wreq
import Control.Lens
import System.Process
import System.Exit
import System.Environment
import Data.Char
import Safe
import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

type URL = T.Text

runWrk :: URL -> IO Double
runWrk uri =
    do (ec, stdout, stderr) <-
           readProcessWithExitCode "wrk" ["-t8", "-c400", "-d30s", T.unpack uri] ""
       case ec of
         ExitFailure failCode ->
             fail $
             "running wrk failed with exit code " ++ show failCode ++ ": \n"
             ++ stdout
             ++ "\n"
             ++ stderr
         ExitSuccess -> parseWrkReqPerSec (T.pack stdout)

parseWrkReqPerSec :: Monad m => T.Text -> m Double
parseWrkReqPerSec stdout =
    loop $ T.lines stdout
    where
      loop [] = fail $ "Did not find a Request/sec line in: " ++ show stdout
      loop (x : xs)
          | "Requests/sec:" `T.isInfixOf` x =
                let maybeNumber = T.dropWhile (not . isNumber) x
                in case readMay (T.unpack maybeNumber) of
                    Just ok -> pure ok
                    Nothing -> fail $ "Failed to read " ++ show maybeNumber ++ " as Double"
          | otherwise = loop xs

type WebApp = Int -> IO ()

validateEndpoint :: URL -> T.Text -> IO ()
validateEndpoint url expected =
    do r <- get $ T.unpack url
       let status = r ^. responseStatus . statusCode
       when (status /= 200) $
          fail ("Failed to GET " ++ T.unpack url ++ ". Status: " ++ show status)
       let rbody = T.decodeUtf8 $ BSL.toStrict $ r ^. responseBody
       when (rbody /= expected) $
          fail ("Endpoint " ++ T.unpack url ++ " was expected to return "
                ++ show expected ++ " but gave " ++ show rbody)

benchApp :: String -> WebApp -> IO ()
benchApp str app =
    do logStr "Starting application"
       webServer <- async $ app 4000
       threadDelay 1000000 -- 1sec
       logStr "Validating endpoints"
       validateEndpoint helloEP "Hello world"
       validateEndpoint plusEP "2"
       validateEndpoint deepEP "Found me!"
       logStr "Benchmarking"
       simplePrint <- runWrk helloEP
       logPerf "simple print" simplePrint
       singleParam <- runWrk plusEP
       logPerf "single param" singleParam
       deepNested <- runWrk deepEP
       logPerf "deeply nested route" deepNested
       cancel webServer
       logStr "Cooldown of 2 seconds"
       threadDelay 2000000
    where
        helloEP = "http://127.0.0.1:4000/hello"
        plusEP = "http://127.0.0.1:4000/plus/1"
        deepEP = "http://127.0.0.1:4000/deep/5/5/5"
        logStr what =
            putStrLn $ "[" ++ str ++ "] " ++ what
        logPerf what dbl =
            logStr $ what ++ " : " ++ show dbl ++ " req/s"

main :: IO ()
main =
    do args <- getArgs
       let filterFun =
               case args of
                 [] -> Left $ const True
                 ("-h":_) -> Right help
                 ("--help":_) -> Right help
                 ("--list":_) -> Right listAll
                 xs -> Left $ \name -> name `elem` xs
       case filterFun of
         Right io -> io
         Left fltr -> mapM_ (uncurry benchApp) (filter (fltr . fst) benchmarks)
    where
        help =
            do putStrLn "Usage: ./Spock-bench [--help|--list] [benchmark-name]*"
               putStrLn "  - If no benchmark names are given, all benchmarks are run"
               putStrLn "  - Requires wrk installed an in path!"
        listAll =
            mapM_ (putStrLn . fst) benchmarks
        benchmarks =
            [ ("Spock", Spock.runApp)
            , ("Scotty", Scotty.runApp)
            , ("Snap", Snap.runApp)
            , ("Fn", Fn.runApp)
            , ("Servant", Servant.runApp)
            ]
