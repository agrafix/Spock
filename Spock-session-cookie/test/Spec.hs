{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Control.Concurrent.MVar
import Control.Concurrent.Async (concurrently)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Data.Aeson hiding (decode)
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Lazy as BL
import Data.Bits (xor)
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import qualified Data.Vault.Lazy as V
import Network.HTTP.Types
import qualified Network.Wai as W
import qualified Network.Wai.Test as W
import Paths_Spock_session_cookie (getDataFileName)
import Test.Hspec
import qualified Web.Cookie as Cookie
import qualified Web.Spock as S
import Web.Spock.Config
import Web.Spock.Internal.SessionManager
import Web.Spock.Session.Cookie
import qualified Web.Spock.SessionActions.Server as Server

main :: IO ()
main = hspec $ do
  describe "XChaCha20-Poly1305 cookie codec" $ do
    it "round-trips every session field without exposing plaintext" $ do
      token <- csc_encode codec "spockcookie" sample
      decode codec "spockcookie" token `shouldReturn` Just (snapshot sample, False)
      forM_ ["private-user", "session-id", "csrf-secret", "2030"] $ \secret ->
        token `shouldSatisfy` (not . B.isInfixOf secret)
    it "uses a fresh nonce for each encryption of the same session" $ do
      first <- csc_encode codec "spockcookie" sample
      second <- csc_encode codec "spockcookie" sample
      B.take 24 <$> unpack first `shouldNotReturn` B.take 24 (either error id $ B64.decodeUnpadded $ last $ B.split 46 second)
    it "accepts a fixture encrypted independently by libsodium" $ do
      path <- getDataFileName "test/libsodium-vector.json"
      fixture <- eitherDecodeFileStrict' path >>= either fail pure
      token <- maybe (fail "Missing fixture cookie") (pure . T.encodeUtf8) $ parseMaybe (withObject "fixture" (.: "cookie")) fixture
      decode codec "spockcookie" token `shouldReturn`
        Just (("independent-id", "independent-csrf", addUTCTime 60 epoch, "private-user"), False)
    it "rejects changes to every nonce, ciphertext and tag byte" $ do
      token <- csc_encode codec "spockcookie" sample
      packed <- unpack token
      forM_ [0 .. B.length packed - 1] $ \i -> do
        let altered = B.take i packed <> B.singleton (B.index packed i `xor` 1) <> B.drop (i + 1) packed
        decode codec "spockcookie" ("v1.old." <> B64.encodeUnpadded altered) `shouldReturn` Nothing
    it "rejects truncation, garbage, oversized input and alternate wire versions" $ do
      token <- csc_encode codec "spockcookie" sample
      forM_ ["", "invalid", "v2." <> B.drop 3 token, "v1.old.!!!", token <> "=", B.replicate 5000 65] $ \invalid ->
        decode codec "spockcookie" invalid `shouldReturn` Nothing
      forM_ [0 .. B.length token - 1] $ \i -> decode codec "spockcookie" (B.take i token) `shouldReturn` Nothing
    it "authenticates the cookie name and application namespace" $ do
      token <- csc_encode codec "spockcookie" sample
      decode codec "other-cookie" token `shouldReturn` Nothing
      decode (cookieSessionCodec $ ring "other-app" oldKey []) "spockcookie" token `shouldReturn` Nothing
    it "authenticates the key identifier, even when identifiers share key bytes" $ do
      token <- csc_encode codec "spockcookie" sample
      let alias = right $ cookieKey "alias" (B.pack [0 .. 31])
          aliased = cookieSessionCodec $ ring "test-app" alias [oldKey]
      decode aliased "spockcookie" ("v1.alias." <> last (B.split 46 token)) `shouldReturn` Nothing
    it "accepts old keys for rotation and rejects them after removal" $ do
      token <- csc_encode codec "spockcookie" sample
      decode rotating "spockcookie" token `shouldReturn` Just (snapshot sample, True)
      decode newOnly "spockcookie" token `shouldReturn` Nothing
      replacement <- csc_encode rotating "spockcookie" sample
      replacement `shouldSatisfy` B.isPrefixOf "v1.new."
      decode newOnly "spockcookie" replacement `shouldReturn` Just (snapshot sample, False)
    it "validates key lengths, identifiers, namespaces and bounded unique rings" $ do
      forM_ [0, 16, 31, 33, 64] $ \n -> errorOf (cookieKey "valid" (B.replicate n 0)) `shouldBe` Just InvalidKeyLength
      forM_ ["", "key.id", "λ", T.replicate 33 "k"] $ \kid ->
        errorOf (cookieKey kid (B.replicate 32 0)) `shouldBe` Just InvalidKeyId
      errorOf (cookieKeyRing "" oldKey []) `shouldBe` Just InvalidNamespace
      errorOf (cookieKeyRing (T.replicate 129 "a") oldKey []) `shouldBe` Just InvalidNamespace
      errorOf (cookieKeyRing "app" oldKey [oldKey]) `shouldBe` Just DuplicateKeyId
      errorOf (cookieKeyRing "app" oldKey (replicate 8 newKey)) `shouldBe` Just TooManyKeys

  describe "Client session lifecycle" $ do
    it "does no cookie decoding or allocation on an unused on-demand route" $ do
      let unused = ClientSessionCodec (\_ _ -> fail "unexpected encode") (\_ _ -> fail "unexpected decode")
      withApp unused id $ \app _ -> do
        resp <- send app "GET" "/empty" [("Cookie", "spockcookie=garbage")]
        W.simpleBody resp `shouldBe` "guest"
        sessionCookies resp `shouldBe` []
    it "preserves sessions across independent application instances without a store" $
      withApp codec id $ \first _ -> withApp codec id $ \second _ -> do
        resp <- send first "GET" "/set" []
        cookie <- cookieHeader resp
        next <- send second "GET" "/read" [cookie]
        W.simpleBody next `shouldBe` "private-user"
    it "keeps repeated operations in one request consistent and emits one final cookie" $
      withApp codec id $ \app _ -> do
        resp <- send app "GET" "/multiple" []
        W.simpleBody resp `shouldBe` "second"
        length (sessionCookies resp) `shouldBe` 1
        cookie <- cookieHeader resp
        W.simpleBody <$> send app "GET" "/read" [cookie] `shouldReturn` "second"
        W.simpleHeaders resp `shouldContain` [("Set-Cookie", "other=value")]
    it "replaces malformed, tampered and expired cookies with empty sessions" $
      withApp codec id $ \app clock -> do
        resp <- send app "GET" "/set" []
        cookie <- cookieHeader resp
        writeIORef clock (addUTCTime 60 epoch)
        W.simpleBody <$> send app "GET" "/read" [cookie] `shouldReturn` ""
        forM_ ["garbage", "%FF", "v1.old.!", B.replicate 5000 65] $ \bad ->
          W.simpleBody <$> send app "GET" "/read" [("Cookie", "spockcookie=" <> bad)] `shouldReturn` ""
    it "renews sliding expiry on use, but an unused route cannot extend it" $
      withApp codec id $ \app clock -> do
        original <- send app "GET" "/set" [] >>= cookieHeader
        writeIORef clock (addUTCTime 40 epoch)
        unused <- send app "GET" "/empty" [original]
        sessionCookies unused `shouldBe` []
        renewed <- send app "GET" "/read" [original] >>= cookieHeader
        writeIORef clock (addUTCTime 60 epoch)
        W.simpleBody <$> send app "GET" "/read" [original] `shouldReturn` ""
        W.simpleBody <$> send app "GET" "/read" [renewed] `shouldReturn` "private-user"
    it "preserves fixed expiry across reads and writes" $
      withApp codec (sessions $ \s -> s { sc_sessionExpandTTL = False }) $ \app clock -> do
        original <- send app "GET" "/set" [] >>= cookieHeader
        writeIORef clock (addUTCTime 40 epoch)
        readBack <- send app "GET" "/read" [original]
        sessionCookies readBack `shouldBe` []
        changed <- send app "GET" "/multiple" [original] >>= cookieHeader
        writeIORef clock (addUTCTime 60 epoch)
        W.simpleBody <$> send app "GET" "/read" [changed] `shouldReturn` ""
    it "rotates an old-key cookie without extending fixed expiry" $
      withApp codec fixed $ \old _ -> withApp rotating fixed $ \new clock -> do
        original <- send old "GET" "/set" [] >>= cookieHeader
        writeIORef clock (addUTCTime 40 epoch)
        refreshed <- send new "GET" "/read" [original]
        token <- cookieValue refreshed
        token `shouldSatisfy` B.isPrefixOf "v1.new."
        fmap (fmap (\(s, _) -> sess_validUntil s)) (csc_decode newOnly "spockcookie" token) `shouldReturn` Just (addUTCTime 60 epoch)
        cookie <- cookieHeader refreshed
        writeIORef clock (addUTCTime 60 epoch)
        W.simpleBody <$> send new "GET" "/read" [cookie] `shouldReturn` ""
    it "regenerates ID and CSRF token while preserving the payload" $
      withApp codec id $ \app _ -> do
        resp <- send app "GET" "/set" []
        original <- decodedCookie resp
        cookie <- cookieHeader resp
        refreshed <- send app "GET" "/regenerate" [cookie] >>= decodedCookie
        sess_id refreshed `shouldNotBe` sess_id original
        sess_csrfToken refreshed `shouldNotBe` sess_csrfToken original
        sess_data refreshed `shouldBe` "private-user"
    it "expires the cookie on logout without replacement, and documents old-cookie replay" $
      withApp codec id $ \app clock -> do
        original <- send app "GET" "/set" [] >>= cookieHeader
        loggedOut <- send app "POST" "/logout" [original]
        length (sessionCookies loggedOut) `shouldBe` 1
        all (B.isInfixOf "Max-Age=0") (sessionCookies loggedOut) `shouldBe` True
        W.simpleBody <$> send app "GET" "/read" [] `shouldReturn` ""
        W.simpleBody <$> send app "GET" "/read" [original] `shouldReturn` "private-user"
        writeIORef clock (addUTCTime 60 epoch)
        W.simpleBody <$> send app "GET" "/read" [original] `shouldReturn` ""
    it "does not reload the incoming cookie after logout within the same request" $
      withApp codec id $ \app _ -> do
        original <- send app "GET" "/set" [] >>= cookieHeader
        resp <- send app "GET" "/destroy-read" [original]
        W.simpleBody resp `shouldBe` ""
        length (sessionCookies resp) `shouldBe` 1
        sess_data <$> decodedCookie resp `shouldReturn` ""
    it "rejects oversized updates before replacing the last successful state or cookie" $
      withApp codec id $ \app _ -> do
        resp <- send app "GET" "/overflow" []
        W.simpleStatus resp `shouldBe` status500
        sess_data <$> decodedCookie resp `shouldReturn` "private-user"
        all ((<= 4096) . B.length) (sessionCookies resp) `shouldBe` True
    it "counts cookie name and attributes in the configured size budget" $
      withApp codec (sessions $ \s -> s
        { sc_cookieSettings = (sc_cookieSettings s) { cs_path = Just $ B.replicate 3900 97 } }) $ \app _ -> do
        resp <- send app "GET" "/set" []
        W.simpleStatus resp `shouldBe` status500
        sessionCookies resp `shouldBe` []
    it "shows concurrent stateless requests are independent, with no atomic counter guarantee" $
      withApp codec id $ \app _ -> do
        original <- send app "GET" "/increment" [] >>= cookieHeader
        (a, b) <- concurrently (send app "GET" "/increment" [original]) (send app "GET" "/increment" [original])
        W.simpleBody a `shouldBe` "2"
        W.simpleBody b `shouldBe` "2"
    it "supports database and application state without providing a server capability" $
      withApp codec id $ \app _ -> do
        W.simpleBody <$> send app "GET" "/services" [] `shouldReturn` "database:state"
        W.simpleBody <$> send app "GET" "/capability" [] `shouldReturn` "absent"
        resp <- send app "GET" "/require-server" []
        W.simpleStatus resp `shouldBe` status500
        sessionCookies resp `shouldBe` []
    it "offers an explicit server capability for server-backed sessions" $ do
      serverStore <- newStmSessionStore
      withApp codec (sessions $ \s -> s { sc_backend = ServerSessions $ defaultServerSessionCfg serverStore }) $ \app _ -> do
        W.simpleBody <$> send app "GET" "/capability" [] `shouldReturn` "present"
        original <- send app "GET" "/set" [] >>= cookieHeader
        W.simpleStatus <$> send app "GET" "/require-server" [] `shouldReturn` status200
        W.simpleBody <$> send app "GET" "/read" [original] `shouldReturn` ""
    it "supports always-on mode and leaves disabled sessions entirely unavailable" $ do
      withApp codec (sessions $ \s -> s { sc_sessionMode = SessionsAlways }) $ \app _ -> do
        resp <- send app "GET" "/empty" []
        length (sessionCookies resp) `shouldBe` 1
      withApp codec (sessions $ \s -> s { sc_sessionMode = SessionsDisabled }) $ \app _ -> do
        forM_ ["/read", "/set", "/regenerate", "/overflow"] $ \path -> do
          resp <- send app "GET" path []
          W.simpleStatus resp `shouldBe` status500
          sessionCookies resp `shouldBe` []
        W.simpleBody <$> send app "GET" "/services" [] `shouldReturn` "database:state"
        W.simpleBody <$> send app "GET" "/capability" [] `shouldReturn` "absent"
    it "checks CSRF before writes and logout, and rotates the token on regeneration" $
      withApp codec (\cfg -> cfg { spc_csrfProtection = True }) $ \app _ -> do
        first <- send app "GET" "/csrf" []
        cookie <- cookieHeader first
        let token = ("X-Csrf-Token", BL.toStrict $ W.simpleBody first)
        W.simpleStatus <$> send app "POST" "/write" [cookie] `shouldReturn` status403
        W.simpleStatus <$> send app "POST" "/write" [cookie, ("X-Csrf-Token", "wrong")] `shouldReturn` status403
        written <- send app "POST" "/write" [cookie, token]
        W.simpleStatus written `shouldBe` status200
        nextCookie <- cookieHeader written
        rotated <- send app "GET" "/regenerate" [nextCookie]
        freshCookie <- cookieHeader rotated
        W.simpleStatus <$> send app "POST" "/logout" [freshCookie, token] `shouldReturn` status403
        fresh <- decodedCookie rotated
        W.simpleStatus <$> send app "POST" "/logout" [freshCookie, ("X-Csrf-Token", T.encodeUtf8 $ sess_csrfToken fresh)] `shouldReturn` status200
    it "rejects invalid TTL, entropy and cookie size settings at startup" $ do
      forM_ [0, -1] $ \ttl ->
        withApp codec (sessions $ \s -> s { sc_sessionTTL = ttl }) (\_ _ -> pure ()) `shouldThrow` (== InvalidClientSessionConfig)
      forM_ [0, 15, 257] $ \entropy ->
        withApp codec (sessions $ \s -> s { sc_sessionIdEntropy = entropy }) (\_ _ -> pure ()) `shouldThrow` (== InvalidClientSessionConfig)
      forM_ [0, 4097] $ \size ->
        withApp codec (sessions $ \s -> case sc_backend s of
          ClientSessions c -> s { sc_backend = ClientSessions c { csc_maxCookieBytes = size } }
          _ -> s) (\_ _ -> pure ()) `shouldThrow` (== InvalidClientSessionConfig)
    it "requires request middleware rather than creating a process-wide visitor session" $ do
      cfg <- defaultSessionCfg ("" :: T.Text)
      let sif = SessionIf (\_ -> pure Nothing) (\_ -> pure ()) (\_ _ -> pure ()) V.newKey
      withSessionManager (cfg { sc_backend = ClientSessions $ defaultClientSessionCfg codec }) sif $ \manager ->
        sm_readSession manager `shouldThrow` (== ClientSessionOutsideRequest)
  where
    fixed = sessions $ \s -> s { sc_sessionExpandTTL = False }

epoch :: UTCTime
epoch = UTCTime (fromGregorian 2030 1 1) 0

sample :: Session () T.Text ()
sample = Session "session-id" "csrf-secret" (addUTCTime 60 epoch) "private-user"

snapshot :: Session conn T.Text st -> (T.Text, T.Text, UTCTime, T.Text)
snapshot s = (sess_id s, sess_csrfToken s, sess_validUntil s, sess_data s)

right :: Show e => Either e a -> a
right = either (error . show) id

errorOf :: Either e a -> Maybe e
errorOf = either Just (const Nothing)

oldKey, newKey :: CookieKey
oldKey = right $ cookieKey "old" (B.pack [0 .. 31])
newKey = right $ cookieKey "new" (B.pack [32 .. 63])

ring :: T.Text -> CookieKey -> [CookieKey] -> CookieKeyRing
ring namespace primary old = right $ cookieKeyRing namespace primary old

codec, rotating, newOnly :: ClientSessionCodec T.Text
codec = cookieSessionCodec $ ring "test-app" oldKey []
rotating = cookieSessionCodec $ ring "test-app" newKey [oldKey]
newOnly = cookieSessionCodec $ ring "test-app" newKey []

decode :: ClientSessionCodec T.Text -> T.Text -> B.ByteString -> IO (Maybe ((T.Text, T.Text, UTCTime, T.Text), Bool))
decode c name value = fmap (\(s, old) -> (snapshot s, old)) <$> csc_decode c name value

unpack :: B.ByteString -> IO B.ByteString
unpack = either fail pure . B64.decodeUnpadded . last . B.split 46

type Cfg = SpockCfg () T.Text T.Text

sessions :: (SessionCfg () T.Text T.Text -> SessionCfg () T.Text T.Text) -> Cfg -> Cfg
sessions f cfg = cfg { spc_sessionCfg = f (spc_sessionCfg cfg) }

withApp :: ClientSessionCodec T.Text -> (Cfg -> Cfg) -> (W.Application -> IORef UTCTime -> IO a) -> IO a
withApp c adjust action = bracket make (\(_, _, close) -> close) $ \(app, clock, _) -> action app clock
  where
    make = do
      clock <- newIORef epoch
      close <- newEmptyMVar
      cfg <- defaultBrowserSpockCfg "" PCNoDatabase "state"
      let s = spc_sessionCfg cfg
          selected = adjust cfg
            { spc_logError = const $ pure (), spc_csrfProtection = False,
              spc_sessionCfg = s { sc_sessionTTL = 60,
                sc_backend = ClientSessions (defaultClientSessionCfg c) { csc_clock = readIORef clock } } }
      app <- S.spockAsApp $ S.spock selected $ do
        manager <- lift S.getSessMgr
        liftIO $ putMVar close (sm_closeSessionManager manager)
        S.get "empty" $ S.text "guest"
        S.get "csrf" $ S.getCsrfToken >>= S.text
        S.get "set" $ S.writeSession "private-user" >> S.text "ok"
        S.post "write" $ S.writeSession "private-user" >> S.text "ok"
        S.get "read" $ S.readSession >>= S.text
        S.get "multiple" $ do
          S.writeSession "first"
          S.modifySession (const "second")
          S.setRawMultiHeader S.MultiHeaderSetCookie "other=value"
          S.readSession >>= S.text
        S.get "regenerate" $ S.sessionRegenerateId >> S.text "ok"
        S.post "logout" $ S.sessionDestroy >> S.text "ok"
        S.get "destroy-read" $ S.sessionDestroy >> S.readSession >>= S.text
        S.get "overflow" $ S.writeSession "private-user" >> S.writeSession (T.replicate 5000 "x") >> S.text "unexpected"
        S.get "increment" $ S.modifyReadSession (\value -> if value == "1" then "2" else "1") >>= S.text
        S.get "services" $ do
          database <- S.runQuery (\() -> pure "database:")
          state <- S.getState
          S.text (database <> state)
        S.get "capability" $ Server.getServerSessionManager >>= S.text . maybe "absent" (const "present")
        S.get "require-server" $ do
          server <- Server.requireServerSessionManager
          Server.mapAllSessions server (const $ pure "mapped")
          Server.clearAllSessions server
          S.text "ok"
      shutdown <- takeMVar close
      pure (app, clock, shutdown)

send :: W.Application -> Method -> B.ByteString -> RequestHeaders -> IO W.SResponse
send app method path headers = W.runSession (W.srequest $ W.SRequest
  ((W.setPath W.defaultRequest path) { W.requestMethod = method, W.requestHeaders = headers }) "") app

sessionCookies :: W.SResponse -> [B.ByteString]
sessionCookies = filter (B.isPrefixOf "spockcookie=") . map snd . filter ((== hSetCookie) . fst) . W.simpleHeaders

cookieValue :: W.SResponse -> IO B.ByteString
cookieValue resp = case sessionCookies resp of
  [value] -> pure $ Cookie.setCookieValue $ Cookie.parseSetCookie value
  _ -> fail "Expected one session cookie"

cookieHeader :: W.SResponse -> IO Header
cookieHeader resp = (,) hCookie . ("spockcookie=" <>) <$> cookieValue resp

decodedCookie :: W.SResponse -> IO (Session () T.Text ())
decodedCookie resp = do
  token <- cookieValue resp
  decoded <- csc_decode codec "spockcookie" token
  maybe (fail "Invalid response cookie") (pure . fst) decoded
