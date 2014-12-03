{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Web.Spock.Internal.CoreAction
    ( ActionT
    , UploadedFile (..)
    , request, header, cookie, body, jsonBody, jsonBody'
    , files, params, param, param', setStatus, setHeader, redirect
    , jumpNext, middlewarePass, modifyVault, queryVault
    , setCookie, setCookie'
    , bytes, lazyBytes, text, html, file, json, blaze
    , requireBasicAuth
    , preferredFormat, ClientPreferredFormat(..)
    )
where

import Web.Spock.Internal.Wire
import Web.Spock.Internal.Util

import Control.Arrow (first)
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State hiding (get, put)
import Data.Monoid
import Data.Time
import Network.HTTP.Types.Status
import Prelude hiding (head)
import System.Locale
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
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
request :: MonadIO m => ActionT m Wai.Request
request = asks ri_request

-- | Read a header
header :: MonadIO m => T.Text -> ActionT m (Maybe T.Text)
header t =
    do req <- request
       return $ fmap T.decodeUtf8 (lookup (CI.mk (T.encodeUtf8 t)) $ Wai.requestHeaders req)

-- | Read a cookie
cookie :: MonadIO m => T.Text -> ActionT m (Maybe T.Text)
cookie name =
    do req <- request
       return $ lookup "cookie" (Wai.requestHeaders req) >>= lookup name . parseCookies . T.decodeUtf8
    where
      parseCookies :: T.Text -> [(T.Text, T.Text)]
      parseCookies = map parseCookie . T.splitOn ";" . T.concat . T.words
      parseCookie = first T.init . T.breakOnEnd "="

-- | Tries to dected the preferred format of the response using the Accept header
preferredFormat :: MonadIO m => ActionT m ClientPreferredFormat
preferredFormat =
  do mAccept <- header "accept"
     case mAccept of
       Nothing -> return PrefUnknown
       Just t ->
         return $ detectPreferredFormat t

-- | Get the raw request body
body :: MonadIO m => ActionT m BS.ByteString
body =
    do req <- request
       let parseBody = liftIO $ Wai.requestBody req
           parseAll chunks =
               do bs <- parseBody
                  if BS.null bs
                  then return chunks
                  else parseAll (chunks `BS.append` bs)
       parseAll BS.empty

-- | Parse the request body as json
jsonBody :: (MonadIO m, A.FromJSON a) => ActionT m (Maybe a)
jsonBody =
    do b <- body
       return $ A.decodeStrict b

-- | Parse the request body as json and fails with 500 status code on error
jsonBody' :: (MonadIO m, A.FromJSON a) => ActionT m a
jsonBody' =
    do b <- body
       case A.eitherDecodeStrict' b of
         Left err ->
             do setStatus status500
                text (T.pack $ "Failed to parse json: " ++ err)
         Right val ->
             return val

-- | Get uploaded files
files :: MonadIO m => ActionT m (HM.HashMap T.Text UploadedFile)
files =
    asks ri_files

-- | Get all request params
params :: MonadIO m => ActionT m [(T.Text, T.Text)]
params =
    do p <- asks ri_params
       qp <- asks ri_queryParams
       return (qp ++ (map (\(k, v) -> (unCaptureVar k, v)) $ HM.toList p))

-- | Read a request param. Spock looks in route captures first, then in POST variables and at last in GET variables
param :: (PathPiece p, MonadIO m) => T.Text -> ActionT m (Maybe p)
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

-- | Like 'param', but outputs an error when a param is missing
param' :: (PathPiece p, MonadIO m) => T.Text -> ActionT m p
param' k =
    do mParam <- param k
       case mParam of
         Nothing ->
             do setStatus status500
                text (T.concat [ "Missing parameter ", k ])
         Just val ->
             return val

-- | Set a response status
setStatus :: MonadIO m => Status -> ActionT m ()
setStatus s =
    modify $ \rs -> rs { rs_status = s }

-- | Set a response header. Overwrites already defined headers
setHeader :: MonadIO m => T.Text -> T.Text -> ActionT m ()
setHeader k v =
    modify $ \rs -> rs { rs_responseHeaders = ((k, v) : filter ((/= k) . fst) (rs_responseHeaders rs)) }

-- | Abort the current action and jump the next one matching the route
jumpNext :: MonadIO m => ActionT m a
jumpNext = throwError ActionTryNext

-- | Redirect to a given url
redirect :: MonadIO m => T.Text -> ActionT m a
redirect = throwError . ActionRedirect

-- | If the Spock application is used as a middleware, you can use
-- this to pass request handeling to the underlying application.
-- If Spock is not uses as a middleware, or there is no underlying application
-- this will result in 404 error.
middlewarePass :: MonadIO m => ActionT m a
middlewarePass = throwError ActionMiddlewarePass

-- | Modify the vault (useful for sharing data between middleware and app)
modifyVault :: MonadIO m => (V.Vault -> V.Vault) -> ActionT m ()
modifyVault f =
    do vaultIf <- asks ri_vaultIf
       liftIO $ (vi_modifyVault vaultIf) f

-- | Query the vault
queryVault :: MonadIO m => V.Key a -> ActionT m (Maybe a)
queryVault k =
    do vaultIf <- asks ri_vaultIf
       liftIO $ (vi_lookupKey vaultIf) k

-- | Set a cookie living for a given number of seconds
setCookie :: MonadIO m => T.Text -> T.Text -> NominalDiffTime -> ActionT m ()
setCookie name value validSeconds =
    do now <- liftIO getCurrentTime
       setCookie' name value (validSeconds `addUTCTime` now)

-- | Set a cookie living until a specific 'UTCTime'
setCookie' :: MonadIO m => T.Text -> T.Text -> UTCTime -> ActionT m ()
setCookie' name value validUntil =
    setHeader "Set-Cookie" rendered
    where
      rendered =
          let formattedTime =
                  T.pack $ formatTime defaultTimeLocale "%a, %d-%b-%Y %X %Z" validUntil
          in T.concat [ name
                      , "="
                      , value
                      , "; path=/; expires="
                      , formattedTime
                      , ";"
                      ]

-- | Send a 'ByteString' as response body. Provide your own "Content-Type"
bytes :: MonadIO m => BS.ByteString -> ActionT m a
bytes val =
    lazyBytes $ BSL.fromStrict val

-- | Send a lazy 'ByteString' as response body. Provide your own "Content-Type"
lazyBytes :: MonadIO m => BSL.ByteString -> ActionT m a
lazyBytes val =
    do modify $ \rs -> rs { rs_responseBody = ResponseLBS val }
       throwError ActionDone

-- | Send text as a response body. Content-Type will be "text/plain"
text :: MonadIO m => T.Text -> ActionT m a
text val =
    do setHeader "Content-Type" "text/plain; charset=utf-8"
       bytes $ T.encodeUtf8 val

-- | Send a text as response body. Content-Type will be "text/html"
html :: MonadIO m => T.Text -> ActionT m a
html val =
    do setHeader "Content-Type" "text/html; charset=utf-8"
       bytes $ T.encodeUtf8 val

-- | Send a file as response
file :: MonadIO m => T.Text -> FilePath -> ActionT m a
file contentType filePath =
     do setHeader "Content-Type" contentType
        modify $ \rs -> rs { rs_responseBody = ResponseFile filePath }
        throwError ActionDone

-- | Send json as response. Content-Type will be "application/json"
json :: (A.ToJSON a, MonadIO m) => a -> ActionT m b
json val =
    do setHeader "Content-Type" "application/json; charset=utf-8"
       lazyBytes $ A.encode val

-- | Send blaze html as response. Content-Type will be "text/html"
blaze :: MonadIO m => Html -> ActionT m a
blaze val =
    do setHeader "Content-Type" "text/html; charset=utf-8"
       lazyBytes $ renderHtml val

-- | Basic authentification
-- provide a title for the prompt and a function to validate
-- user and password. Usage example:
--
-- > get "/my-secret-page" $
-- >   requireBasicAuth "Secret Page" (\user pass -> return (user == "admin" && pass == "1234")) $
-- >   do html "This is top secret content. Login using that secret code I provided ;-)"
--
requireBasicAuth :: MonadIO m => T.Text -> (T.Text -> T.Text -> m Bool) -> ActionT m a -> ActionT m a
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
             setHeader "WWW-Authenticate" ("Basic realm=\"" <> realmTitle <> "\"")
             html "<h1>Authentication required.</h1>"
