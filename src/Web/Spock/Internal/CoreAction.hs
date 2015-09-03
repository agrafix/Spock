{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RecordWildCards #-}
module Web.Spock.Internal.CoreAction
    ( ActionT
    , UploadedFile (..)
    , request, header, rawHeader, cookie, body, jsonBody, jsonBody'
    , reqMethod
    , files, params, param, param', setStatus, setHeader, redirect
    , CookieSettings(..), CookieEOL(..), defaultCookieSettings
    , setCookie, deleteCookie
    , jumpNext, middlewarePass, modifyVault, queryVault
    , bytes, lazyBytes, text, html, file, json, stream, response
    , requireBasicAuth
    , getContext, runInContext
    , preferredFormat, ClientPreferredFormat(..)
    )
where

import Web.Spock.Internal.Cookies
import Web.Spock.Internal.Util
import Web.Spock.Internal.Wire

import Control.Arrow (first)
import Control.Monad
#if MIN_VERSION_mtl(2,2,0)
import Control.Monad.Except
#else
import Control.Monad.Error
#endif
import Control.Monad.Reader
import Control.Monad.RWS.Strict (runRWST)
import Control.Monad.State hiding (get, put)
import qualified Control.Monad.State as ST
import Data.Monoid
import Data.Time
import Network.HTTP.Types.Header (HeaderName, ResponseHeaders)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Prelude hiding (head)
import Web.PathPieces
import Web.Routing.AbstractRouter
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vault.Lazy as V
import qualified Network.Wai as Wai

-- | Get the original Wai Request object
request :: MonadIO m => ActionCtxT ctx m Wai.Request
request = asks ri_request
{-# INLINE request #-}

-- | Read a header
header :: MonadIO m => T.Text -> ActionCtxT ctx m (Maybe T.Text)
header t =
    liftM (fmap T.decodeUtf8) $ rawHeader (CI.mk (T.encodeUtf8 t))
{-# INLINE header #-}

-- | Read a header without converting it to text
rawHeader :: MonadIO m => HeaderName -> ActionCtxT ctx m (Maybe BS.ByteString)
rawHeader t =
    liftM (lookup t . Wai.requestHeaders) request
{-# INLINE rawHeader #-}

-- | Read a cookie
cookie :: MonadIO m => T.Text -> ActionCtxT ctx m (Maybe T.Text)
cookie name =
    do req <- request
       return $ lookup "cookie" (Wai.requestHeaders req) >>= lookup name . parseCookies
{-# INLINE cookie #-}

-- | Tries to dected the preferred format of the response using the Accept header
preferredFormat :: MonadIO m => ActionCtxT ctx m ClientPreferredFormat
preferredFormat =
  do mAccept <- header "accept"
     case mAccept of
       Nothing -> return PrefUnknown
       Just t ->
         return $ detectPreferredFormat t
{-# INLINE preferredFormat #-}

-- | Returns the current request method, e.g. 'GET'
reqMethod :: MonadIO m => ActionCtxT ctx m StdMethod
reqMethod = asks ri_method
{-# INLINE reqMethod #-}

-- | Get the raw request body
body :: MonadIO m => ActionCtxT ctx m BS.ByteString
body =
    do req <- request
       let parseBody = liftIO $ Wai.requestBody req
           parseAll chunks =
               do bs <- parseBody
                  if BS.null bs
                  then return chunks
                  else parseAll (chunks `BS.append` bs)
       parseAll BS.empty
{-# INLINE body #-}

-- | Parse the request body as json
jsonBody :: (MonadIO m, A.FromJSON a) => ActionCtxT ctx m (Maybe a)
jsonBody =
    do b <- body
       return $ A.decodeStrict b
{-# INLINE jsonBody #-}

-- | Parse the request body as json and fails with 500 status code on error
jsonBody' :: (MonadIO m, A.FromJSON a) => ActionCtxT ctx m a
jsonBody' =
    do b <- body
       case A.eitherDecodeStrict' b of
         Left err ->
             do setStatus status500
                text (T.pack $ "Failed to parse json: " ++ err)
         Right val ->
             return val
{-# INLINE jsonBody' #-}

-- | Get uploaded files
files :: MonadIO m => ActionCtxT ctx m (HM.HashMap T.Text UploadedFile)
files =
    asks ri_files
{-# INLINE files #-}

-- | Get all request params
params :: MonadIO m => ActionCtxT ctx m [(T.Text, T.Text)]
params =
    do p <- asks ri_params
       qp <- asks ri_queryParams
       return (qp ++ map (first unCaptureVar) (HM.toList p))
{-# INLINE params #-}

-- | Read a request param. Spock looks in route captures first (in simple routing), then in POST variables and at last in GET variables
param :: (PathPiece p, MonadIO m) => T.Text -> ActionCtxT ctx m (Maybe p)
param k =
    do p <- asks ri_params
       qp <- asks ri_queryParams
       case HM.lookup (CaptureVar k) p of
         Just val ->
             case fromPathPiece val of
               Nothing ->
                   do liftIO $ putStrLn ("Cannot parse " ++ show k ++ " with value " ++ show val ++ " as path piece!")
                      jumpNext
               Just pathPieceVal ->
                   return $ Just pathPieceVal
         Nothing ->
             return $ join $ fmap fromPathPiece (lookup k qp)
{-# INLINE param #-}

-- | Like 'param', but outputs an error when a param is missing
param' :: (PathPiece p, MonadIO m) => T.Text -> ActionCtxT ctx m p
param' k =
    do mParam <- param k
       case mParam of
         Nothing ->
             do setStatus status500
                text (T.concat [ "Missing parameter ", k ])
         Just val ->
             return val
{-# INLINE param' #-}

-- | Set a response status
setStatus :: MonadIO m => Status -> ActionCtxT ctx m ()
setStatus s =
    modify $ \rs -> rs { rs_status = s }
{-# INLINE setStatus #-}

-- | Set a response header. If the response header
-- is allowed to occur multiple times (as in RFC 2616), it will
-- be appended. Otherwise the previous value is overwritten.
-- See 'setMultiHeader'.
setHeader :: MonadIO m => T.Text -> T.Text -> ActionCtxT ctx m ()
setHeader k v =
    do let ciVal = CI.mk $ T.encodeUtf8 k
       case HM.lookup ciVal multiHeaderMap of
         Just mhk ->
             setMultiHeader mhk v
         Nothing ->
             setHeaderUnsafe k v
{-# INLINE setHeader #-}

-- | INTERNAL: Set a response header that can occur multiple times. (eg: Cache-Control)
setMultiHeader :: MonadIO m => MultiHeader -> T.Text -> ActionCtxT ctx m ()
setMultiHeader k v =
    modify $ \rs ->
        rs
        { rs_multiResponseHeaders =
              HM.insertWith (++) k [T.encodeUtf8 v] (rs_multiResponseHeaders rs)
        }
{-# INLINE setMultiHeader #-}

-- | INTERNAL: Unsafely set a header (no checking if the header can occur multiple times)
setHeaderUnsafe :: MonadIO m => T.Text -> T.Text -> ActionCtxT ctx m ()
setHeaderUnsafe k v =
    modify $ \rs ->
        rs
        { rs_responseHeaders =
              HM.insert (CI.mk $ T.encodeUtf8 k) (T.encodeUtf8 v) (rs_responseHeaders rs)
        }
{-# INLINE setHeaderUnsafe #-}


-- | Abort the current action and jump the next one matching the route
jumpNext :: MonadIO m => ActionCtxT ctx m a
jumpNext = throwError ActionTryNext
{-# INLINE jumpNext #-}

-- | Redirect to a given url
redirect :: MonadIO m => T.Text -> ActionCtxT ctx m a
redirect = throwError . ActionRedirect
{-# INLINE redirect #-}

-- | If the Spock application is used as a middleware, you can use
-- this to pass request handling to the underlying application.
-- If Spock is not uses as a middleware, or there is no underlying application
-- this will result in 404 error.
middlewarePass :: MonadIO m => ActionCtxT ctx m a
middlewarePass = throwError ActionMiddlewarePass
{-# INLINE middlewarePass #-}

-- | Modify the vault (useful for sharing data between middleware and app)
modifyVault :: MonadIO m => (V.Vault -> V.Vault) -> ActionCtxT ctx m ()
modifyVault f =
    do vaultIf <- asks ri_vaultIf
       liftIO $ vi_modifyVault vaultIf f
{-# INLINE modifyVault #-}

-- | Query the vault
queryVault :: MonadIO m => V.Key a -> ActionCtxT ctx m (Maybe a)
queryVault k =
    do vaultIf <- asks ri_vaultIf
       liftIO $ vi_lookupKey vaultIf k
{-# INLINE queryVault #-}

-- | Use a custom 'Wai.Response' generator as response body.
response :: MonadIO m => (Status -> ResponseHeaders -> Wai.Response) -> ActionCtxT ctx m a
response val =
    do modify $ \rs -> rs { rs_responseBody = ResponseBody val }
       throwError ActionDone
{-# INLINE response #-}

-- | Send a 'ByteString' as response body. Provide your own "Content-Type"
bytes :: MonadIO m => BS.ByteString -> ActionCtxT ctx m a
bytes val =
    lazyBytes $ BSL.fromStrict val
{-# INLINE bytes #-}

-- | Send a lazy 'ByteString' as response body. Provide your own "Content-Type"
lazyBytes :: MonadIO m => BSL.ByteString -> ActionCtxT ctx m a
lazyBytes val =
    response $ \status headers -> Wai.responseLBS status headers val
{-# INLINE lazyBytes #-}

-- | Send text as a response body. Content-Type will be "text/plain"
text :: MonadIO m => T.Text -> ActionCtxT ctx m a
text val =
    do setHeaderUnsafe "Content-Type" "text/plain; charset=utf-8"
       bytes $ T.encodeUtf8 val
{-# INLINE text #-}

-- | Send a text as response body. Content-Type will be "text/html"
html :: MonadIO m => T.Text -> ActionCtxT ctx m a
html val =
    do setHeaderUnsafe "Content-Type" "text/html; charset=utf-8"
       bytes $ T.encodeUtf8 val
{-# INLINE html #-}

-- | Send a file as response
file :: MonadIO m => T.Text -> FilePath -> ActionCtxT ctx m a
file contentType filePath =
     do setHeaderUnsafe "Content-Type" contentType
        response $ \status headers -> Wai.responseFile status headers filePath Nothing
{-# INLINE file #-}

-- | Send json as response. Content-Type will be "application/json"
json :: (A.ToJSON a, MonadIO m) => a -> ActionCtxT ctx m b
json val =
    do setHeaderUnsafe "Content-Type" "application/json; charset=utf-8"
       lazyBytes $ A.encode val
{-# INLINE json #-}

-- | Use a 'Wai.StreamingBody' to generate a response.
stream :: MonadIO m => Wai.StreamingBody -> ActionCtxT ctx m a
stream val =
    response $ \status headers -> Wai.responseStream status headers val
{-# INLINE stream #-}

-- | Basic authentification
-- provide a title for the prompt and a function to validate
-- user and password. Usage example:
--
-- > get "/my-secret-page" $
-- >   requireBasicAuth "Secret Page" (\user pass -> return (user == "admin" && pass == "1234")) $
-- >   do html "This is top secret content. Login using that secret code I provided ;-)"
--
requireBasicAuth :: MonadIO m => T.Text -> (T.Text -> T.Text -> m Bool) -> ActionCtxT ctx m a -> ActionCtxT ctx m a
requireBasicAuth realmTitle authFun cont =
    do mAuthHeader <- header "Authorization"
       case mAuthHeader of
         Nothing ->
             authFailed
         Just authHeader ->
             let (_, rawValue) =
                     T.breakOn " " authHeader
                 (user, rawPass) =
                     (T.breakOn ":" . T.decodeUtf8 . B64.decodeLenient . T.encodeUtf8 . T.strip) rawValue
                 pass = T.drop 1 rawPass
             in do isOk <- lift $ authFun user pass
                   if isOk
                   then cont
                   else authFailed
    where
      authFailed =
          do setStatus status401
             setMultiHeader MultiHeaderWWWAuth ("Basic realm=\"" <> realmTitle <> "\"")
             html "<h1>Authentication required.</h1>"

-- | Get the context of the current request
getContext :: MonadIO m => ActionCtxT ctx m ctx
getContext = asks ri_context
{-# INLINE getContext #-}

-- | Run an Action in a different context
runInContext :: MonadIO m => ctx' -> ActionCtxT ctx' m a -> ActionCtxT ctx m a
runInContext newCtx action =
    do currentEnv <- ask
       currentRespState <- ST.get
       (r, newRespState, _) <-
          lift $
          do let env =
                     currentEnv
                     { ri_context = newCtx
                     }
             runRWST (runErrorT $ runActionCtxT action) env currentRespState
       ST.put newRespState
       case r of
         Left interupt ->
             throwError interupt
         Right d -> return d
{-# INLINE runInContext #-}

setCookie :: MonadIO m => T.Text -> T.Text -> CookieSettings -> ActionCtxT ctx m ()
setCookie name value cs =
    do now <- liftIO getCurrentTime
       let cookieHeaderString = generateCookieHeaderString name value cs now
       setMultiHeader MultiHeaderSetCookie cookieHeaderString
{-# INLINE setCookie #-}

deleteCookie :: MonadIO m => T.Text -> ActionCtxT ctx m ()
deleteCookie name = setCookie name T.empty cs
  where
    cs = defaultCookieSettings { cs_EOL = CookieValidUntil epoch }
    epoch = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)
{-# INLINE deleteCookie #-}

