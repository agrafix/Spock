{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Spock.SessionModeSpec (spec) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Time
import Network.HTTP.Types (HeaderName, status500)
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai
import Test.Hspec
import Web.Spock hiding (request)
import Web.Spock.Config
import Web.Spock.Internal.SessionManager (Session (..))
import Web.Spock.Internal.SessionVault (newStmSessionStore')
import Web.Spock.TestUtils

spec :: Spec
spec = describe "Session modes" $ do
  it "keeps eager allocation as the default" $ do
    cfg <- defaultSessionCfg ()
    sc_sessionMode cfg `shouldBe` SessionsAlways
    (app, store) <- modeApp SessionsAlways
    response <- request app "/empty" []
    getSessCookie response `shouldSatisfy` (/= Nothing)
    length <$> ss_runTx store (ss_toList store) `shouldReturn` 1

  it "does not allocate or set a cookie for an unused on-demand session" $ do
    (app, store) <- modeApp SessionsOnDemand
    response <- request app "/empty" []
    getSessCookie response `shouldBe` Nothing
    length <$> ss_runTx store (ss_toList store) `shouldReturn` 0

  it "does not replace an invalid cookie until a session is used" $ do
    (app, store) <- modeApp SessionsOnDemand
    response <- request app "/empty" [("Cookie", "spockcookie=missing")]
    getSessCookie response `shouldBe` Nothing
    length <$> ss_runTx store (ss_toList store) `shouldReturn` 0

  it "does not renew an unused existing session" $ do
    (app, store) <- modeApp SessionsOnDemand
    now <- getCurrentTime
    let expiry = addUTCTime 60 now
    ss_runTx store $ ss_storeSession store (Session "existing" "token" expiry 3)
    response <- request app "/empty" [("Cookie", "spockcookie=existing")]
    getSessCookie response `shouldBe` Nothing
    fmap sess_validUntil <$> ss_runTx store (ss_loadSession store "existing")
      `shouldReturn` Just expiry

  it "loads and renews an existing session on first use" $ do
    (app, store) <- modeApp SessionsOnDemand
    now <- getCurrentTime
    ss_runTx store $ ss_storeSession store (Session "existing" "token" (addUTCTime 60 now) 3)
    response <- request app "/read" [("Cookie", "spockcookie=existing")]
    Wai.simpleBody response `shouldBe` "3"
    getSessCookie response `shouldBe` Nothing
    stored <- ss_runTx store $ ss_loadSession store "existing"
    fmap sess_validUntil stored `shouldSatisfy` maybe False (> addUTCTime 60 now)

  it "creates a single session for multiple actions and remembers writes" $ do
    (app, store) <- modeApp SessionsOnDemand
    Wai.runSession (do
      response <- Wai.srequest $ Wai.SRequest (Wai.setPath Wai.defaultRequest "/write") ""
      response2 <- Wai.srequest $ Wai.SRequest (Wai.setPath Wai.defaultRequest "/read") ""
      pure (response, response2)) app >>= \(response, response2) -> do
        getSessCookie response `shouldSatisfy` (/= Nothing)
        Wai.simpleBody response `shouldBe` "7"
        Wai.simpleBody response2 `shouldBe` "7"
    length <$> ss_runTx store (ss_toList store) `shouldReturn` 1

  it "creates a session when a CSRF token is requested" $ do
    (app, store) <- modeApp SessionsOnDemand
    response <- request app "/csrf" []
    Wai.simpleBody response `shouldSatisfy` (not . LBS.null)
    getSessCookie response `shouldSatisfy` (/= Nothing)
    length <$> ss_runTx store (ss_toList store) `shouldReturn` 1

  it "replaces an expired on-demand session with the empty value" $ do
    (app, store) <- modeApp SessionsOnDemand
    now <- getCurrentTime
    ss_runTx store $ ss_storeSession store (Session "expired" "token" (addUTCTime (-1) now) 99)
    response <- request app "/read" [("Cookie", "spockcookie=expired")]
    Wai.simpleBody response `shouldBe` "0"
    getSessCookie response `shouldSatisfy` maybe False (/= "expired")
    fmap sess_id <$> ss_runTx store (ss_loadSession store "expired") `shouldReturn` Nothing

  mapM_ (\mode -> it ("preserves application state and database pooling with " ++ show mode) $ do
    (app, _) <- modeApp mode
    response <- request app "/services" []
    Wai.simpleBody response `shouldBe` "connected:state"
    getSessCookie response `shouldBe` Nothing) [SessionsOnDemand, SessionsDisabled]

  it "reports use of disabled sessions as an error without creating a cookie" $ do
    (app, store) <- modeApp SessionsDisabled
    response <- request app "/write" []
    Wai.simpleStatus response `shouldBe` status500
    getSessCookie response `shouldBe` Nothing
    length <$> ss_runTx store (ss_toList store) `shouldReturn` 0

  it "rejects disabled sessions combined with CSRF protection at startup" $ do
    cfg <- defaultSpockCfg () PCNoDatabase ()
    let disabled = cfg { spc_csrfProtection = True,
                         spc_sessionCfg = (spc_sessionCfg cfg) { sc_sessionMode = SessionsDisabled } }
    spock disabled (pure ()) `shouldThrow` (== CsrfRequiresSessions)

type Store = SessionStore (Session T.Text Int T.Text) STM

modeApp :: SessionMode -> IO (Wai.Application, Store)
modeApp mode = do
  store <- newStmSessionStore'
  ready <- newEmptyMVar
  let connection = ConnBuilder (pure "connected") (const $ pure ()) (PoolCfg 1 2 60)
  cfg <- defaultSpockCfg (0 :: Int) (PCConn connection) "state"
  let sessions = (spc_sessionCfg cfg)
        { sc_sessionMode = mode,
          sc_store = SessionStoreInstance store,
          sc_hooks = SessionHooks (const $ putMVar ready ()) }
  app <- spockAsApp $ spock (cfg { spc_sessionCfg = sessions, spc_logError = const $ pure () }) $ do
    get "empty" $ text "guest"
    get "read" $ readSession >>= text . T.pack . show
    get "write" $ writeSession 7 >> readSession >>= text . T.pack . show
    get "csrf" $ getCsrfToken >>= text
    get "services" $ do
      connectionValue <- runQuery pure
      state <- getState
      text $ connectionValue <> ":" <> state
  unless (mode == SessionsDisabled) $ takeMVar ready
  pure (app, store)

request :: Wai.Application -> BS.ByteString -> [(HeaderName, BS.ByteString)] -> IO Wai.SResponse
request app path headers =
  Wai.runSession (Wai.srequest $ Wai.SRequest ((Wai.setPath Wai.defaultRequest path) { Wai.requestHeaders = headers }) "") app
