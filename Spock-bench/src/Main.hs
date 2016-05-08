{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Spock
import qualified Scotty
import qualified Snap
import qualified Fn

import System.Process
import System.Exit
import System.Environment
import Data.Char
import Safe
import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.Text as T

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

benchApp :: String -> WebApp -> IO ()
benchApp str app =
    do logStr "Starting benchmark"
       webServer <- async $ app 4000
       threadDelay 1000000 -- 1sec
       simplePrint <- runWrk "http://127.0.0.1:4000/hello"
       logPerf "simple print" simplePrint
       singleParam <- runWrk "http://127.0.0.1:4000/plus/1"
       logPerf "single param" singleParam
       deepNested <- runWrk "http://127.0.0.1:4000/deep/10/10/10"
       logPerf "deeply nested route" deepNested
       cancel webServer
    where
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
            ]
