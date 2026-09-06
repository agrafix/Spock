{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (bracket)
import Data.Pool
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Web.Spock
import Web.Spock.Config
import Web.Spock.Session.Postgresql

-- Uses libpq's PGHOST, PGPORT, PGUSER, PGDATABASE, and password-file settings.
main :: IO ()
main = bracket createPool destroyAllResources $ \pool -> do
  withResource pool initializePostgresqlSessions
  store <- newPostgresqlSessionStore defaultPostgresqlSessionCfg pool
  cfg <- defaultBrowserSpockCfg (0 :: Int) PCNoDatabase ()
  let sessions = (spc_sessionCfg cfg)
        { sc_backend = ServerSessions $ defaultServerSessionCfg $ SessionStoreInstance store,
          -- This example listens on local HTTP. Use True with HTTPS in production.
          sc_cookieSettings = (sc_cookieSettings $ spc_sessionCfg cfg) { cs_secure = False } }
  runSpock 8080 $ spock (cfg { spc_sessionCfg = sessions }) $ do
    get root $ text "GET /csrf for a token; POST /increment with X-Csrf-Token; POST /logout to reset."
    get "csrf" $ getCsrfToken >>= text
    get "count" $ readSession >>= text . T.pack . show
    post "increment" $ modifyReadSession (+ 1) >>= text . T.pack . show
    post "logout" $ sessionDestroy >> text "Logged out"
  where
    createPool = newPool $ defaultPoolConfig (connectPostgreSQL "") close 60 10
