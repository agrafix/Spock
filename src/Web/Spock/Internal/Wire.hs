{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Web.Spock.Internal.Wire where

import Control.Arrow ((***))
import Control.Applicative
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.RWS.Strict
#if MIN_VERSION_mtl(2,2,0)
import Control.Monad.Except
#else
import Control.Monad.Error
#endif
import Control.Monad.Reader.Class ()
import Control.Monad.Trans.Resource
import Data.Hashable
import Data.IORef
import Data.Maybe
import Data.Typeable
import Data.Word
import GHC.Generics
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
#if MIN_VERSION_base(4,6,0)
import Prelude
#else
import Prelude hiding (catch)
#endif
import System.Directory
import Web.Routing.AbstractRouter
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vault.Lazy as V
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as P

instance Hashable StdMethod where
    hashWithSalt = hashUsing fromEnum

data UploadedFile
   = UploadedFile
   { uf_name :: !T.Text
   , uf_contentType :: !T.Text
   , uf_tempLocation :: !FilePath
   }

data VaultIf
   = VaultIf
   { vi_modifyVault :: (V.Vault -> V.Vault) -> IO ()
   , vi_lookupKey :: forall a. V.Key a -> IO (Maybe a)
   }

data RequestInfo ctx
   = RequestInfo
   { ri_method :: !StdMethod
   , ri_request :: !Wai.Request
   , ri_params :: !(HM.HashMap CaptureVar T.Text)
   , ri_queryParams :: [(T.Text, T.Text)]
   , ri_files :: !(HM.HashMap T.Text UploadedFile)
   , ri_vaultIf :: !VaultIf
   , ri_context :: !ctx
   }

newtype ResponseBody = ResponseBody (Status -> ResponseHeaders -> Wai.Response)

data MultiHeader
   = MultiHeaderCacheControl
   | MultiHeaderConnection
   | MultiHeaderContentEncoding
   | MultiHeaderContentLanguage
   | MultiHeaderPragma
   | MultiHeaderProxyAuthenticate
   | MultiHeaderTrailer
   | MultiHeaderTransferEncoding
   | MultiHeaderUpgrade
   | MultiHeaderVia
   | MultiHeaderWarning
   | MultiHeaderWWWAuth
   | MultiHeaderSetCookie
     deriving (Show, Eq, Enum, Bounded, Generic)

instance Hashable MultiHeader

multiHeaderCI :: MultiHeader -> CI.CI BS.ByteString
multiHeaderCI mh =
    case mh of
      MultiHeaderCacheControl -> "Cache-Control"
      MultiHeaderConnection -> "Connection"
      MultiHeaderContentEncoding -> "Content-Encoding"
      MultiHeaderContentLanguage -> "Content-Language"
      MultiHeaderPragma -> "Pragma"
      MultiHeaderProxyAuthenticate -> "Proxy-Authenticate"
      MultiHeaderTrailer -> "Trailer"
      MultiHeaderTransferEncoding -> "Transfer-Encoding"
      MultiHeaderUpgrade -> "Upgrade"
      MultiHeaderVia -> "Via"
      MultiHeaderWarning -> "Warning"
      MultiHeaderWWWAuth -> "WWW-Authenticate"
      MultiHeaderSetCookie -> "Set-Cookie"

multiHeaderMap :: HM.HashMap (CI.CI BS.ByteString) MultiHeader
multiHeaderMap =
    HM.fromList $ flip map allHeaders $ \mh ->
    (multiHeaderCI mh, mh)
    where
      -- this is a nasty hack until we know more about the origin of
      -- uncaught exception: ErrorCall (toEnum{MultiHeader}: tag (-12565) is outside of enumeration's range (0,12))
      -- see: https://ghc.haskell.org/trac/ghc/ticket/10792 and https://github.com/agrafix/Spock/issues/44
      allHeaders =
          [ MultiHeaderCacheControl
          , MultiHeaderConnection
          , MultiHeaderContentEncoding
          , MultiHeaderContentLanguage
          , MultiHeaderPragma
          , MultiHeaderProxyAuthenticate
          , MultiHeaderTrailer
          , MultiHeaderTransferEncoding
          , MultiHeaderUpgrade
          , MultiHeaderVia
          , MultiHeaderWarning
          , MultiHeaderWWWAuth
          , MultiHeaderSetCookie
          ]

data ResponseState
   = ResponseState
   { rs_responseHeaders :: !(HM.HashMap (CI.CI BS.ByteString) BS.ByteString)
   , rs_multiResponseHeaders :: !(HM.HashMap MultiHeader [BS.ByteString])
   , rs_status :: !Status
   , rs_responseBody :: !ResponseBody
   }

data ActionInterupt
    = ActionRedirect !T.Text
    | ActionTryNext
    | ActionError String
    | ActionDone
    | ActionMiddlewarePass
    deriving (Show, Typeable)

instance Monoid ActionInterupt where
    mempty = ActionDone
    mappend _ a = a

#if MIN_VERSION_mtl(2,2,0)
type ErrorT = ExceptT
runErrorT :: ExceptT e m a -> m (Either e a)
runErrorT = runExceptT
#else
instance Error ActionInterupt where
    noMsg = ActionError "Unkown Internal Action Error"
    strMsg = ActionError
#endif

type ActionT = ActionCtxT ()

newtype ActionCtxT ctx m a
    = ActionCtxT { runActionCtxT :: ErrorT ActionInterupt (RWST (RequestInfo ctx) () ResponseState m) a }
      deriving (Monad, Functor, Applicative, Alternative, MonadIO, MonadReader (RequestInfo ctx), MonadState ResponseState, MonadError ActionInterupt)

instance MonadTrans (ActionCtxT ctx) where
    lift = ActionCtxT . lift . lift

respStateToResponse :: ResponseState -> Wai.Response
respStateToResponse (ResponseState headers multiHeaders status (ResponseBody body)) =
    let mkMultiHeader (k, vals) =
            let kCi = multiHeaderCI k
            in map (\v -> (kCi, v)) vals
        outHeaders =
            HM.toList headers
            ++ (concatMap mkMultiHeader $ HM.toList multiHeaders)
    in body status outHeaders

errorResponse :: Status -> BSL.ByteString -> ResponseState
errorResponse s e =
    ResponseState
    { rs_responseHeaders =
          HM.singleton "Content-Type" "text/html"
    , rs_multiResponseHeaders =
          HM.empty
    , rs_status = s
    , rs_responseBody = ResponseBody $ \status headers ->
        Wai.responseLBS status headers $
        BSL.concat [ "<html><head><title>"
                   , e
                   , "</title></head><body><h1>"
                   , e
                   , "</h1></body></html>"
                   ]
    }

defResponse :: ResponseState
defResponse =
    ResponseState
    { rs_responseHeaders =
          HM.empty
    , rs_multiResponseHeaders =
          HM.empty
    , rs_status = status200
    , rs_responseBody = ResponseBody $ \status headers ->
        Wai.responseLBS status headers $
        BSL.empty
    }

notFound :: Wai.Response
notFound =
    respStateToResponse $ errorResponse status404 "404 - File not found"

invalidReq :: Wai.Response
invalidReq =
    respStateToResponse $ errorResponse status400 "400 - Bad request"

serverError :: ResponseState
serverError =
    errorResponse status500 "500 - Internal Server Error!"

sizeError :: ResponseState
sizeError =
    errorResponse status413 "413 - Request body too large!"

type SpockAllT r m a =
    RegistryT r Wai.Middleware StdMethod m a

middlewareToApp :: Wai.Middleware
                -> Wai.Application
middlewareToApp mw =
    mw fallbackApp
    where
      fallbackApp :: Wai.Application
      fallbackApp _ respond = respond notFound

makeActionEnvironment :: InternalState -> StdMethod -> Wai.Request -> IO (ParamMap -> RequestInfo (), TVar V.Vault, IO ())
makeActionEnvironment st stdMethod req =
    do (bodyParams, bodyFiles) <- P.parseRequestBody (P.tempFileBackEnd st) req
       vaultVar <- liftIO $ newTVarIO (Wai.vault req)
       let vaultIf =
               VaultIf
               { vi_modifyVault = atomically . modifyTVar' vaultVar
               , vi_lookupKey = \k -> V.lookup k <$> atomically (readTVar vaultVar)
               }
           uploadedFiles =
               HM.fromList $
                 map (\(k, fileInfo) ->
                          ( T.decodeUtf8 k
                          , UploadedFile (T.decodeUtf8 $ P.fileName fileInfo) (T.decodeUtf8 $ P.fileContentType fileInfo) (P.fileContent fileInfo)
                          )
                     ) bodyFiles
           postParams =
               map (T.decodeUtf8 *** T.decodeUtf8) bodyParams
           getParams =
               map (\(k, mV) -> (T.decodeUtf8 k, T.decodeUtf8 $ fromMaybe BS.empty mV)) $ Wai.queryString req
           queryParams = postParams ++ getParams
       return ( \params ->
                    RequestInfo
                    { ri_method = stdMethod
                    , ri_request = req
                    , ri_params = params
                    , ri_queryParams = queryParams
                    , ri_files = uploadedFiles
                    , ri_vaultIf = vaultIf
                    , ri_context = ()
                    }
              , vaultVar
              , removeUploadedFiles uploadedFiles
              )

removeUploadedFiles :: HM.HashMap k UploadedFile -> IO ()
removeUploadedFiles uploadedFiles =
    forM_ (HM.elems uploadedFiles) $ \uploadedFile ->
    do stillThere <- doesFileExist (uf_tempLocation uploadedFile)
       when stillThere $ liftIO $ removeFile (uf_tempLocation uploadedFile)

applyAction :: MonadIO m
            => Wai.Request
            -> (ParamMap -> RequestInfo ())
            -> [(ParamMap, ActionT m ())]
            -> m (Maybe ResponseState)
applyAction _ _ [] =
    return $ Just $ errorResponse status404 "404 - File not found"
applyAction req mkEnv ((captures, selectedAction) : xs) =
    do let env = mkEnv captures
       (r, respState, _) <-
           runRWST (runErrorT $ runActionCtxT selectedAction) env defResponse
       case r of
         Left (ActionRedirect loc) ->
             return $ Just $
                    respState
                    { rs_status = status302
                    , rs_responseBody =
                        ResponseBody $ \status headers ->
                            Wai.responseLBS status (("Location", T.encodeUtf8 loc) : headers) BSL.empty
                    }
         Left ActionTryNext ->
             applyAction req mkEnv xs
         Left (ActionError errorMsg) ->
             do liftIO $ putStrLn $ "Spock Error while handling "
                             ++ show (Wai.pathInfo req) ++ ": " ++ errorMsg
                return $ Just serverError
         Left ActionDone ->
             return $ Just respState
         Left ActionMiddlewarePass ->
             return Nothing
         Right () ->
             return $ Just respState

handleRequest
    :: MonadIO m
    => StdMethod
    -> Maybe Word64
    -> (forall a. m a -> IO a)
    -> [(ParamMap, ActionT m ())]
    -> InternalState
    -> Wai.Application -> Wai.Application
handleRequest stdMethod mLimit registryLift allActions st coreApp req respond =
    do reqGo <-
           case mLimit of
             Nothing -> return req
             Just lim -> requestSizeCheck lim req
       handleRequest' stdMethod registryLift allActions st coreApp reqGo respond

handleRequest' ::
    MonadIO m
    => StdMethod
    -> (forall a. m a -> IO a)
    -> [(ParamMap, ActionT m ())]
    -> InternalState
    -> Wai.Application -> Wai.Application
handleRequest' stdMethod registryLift allActions st coreApp req respond =
    do actEnv <-
           (Left <$> makeActionEnvironment st stdMethod req)
           `catch` \(_ :: SizeException) ->
               return (Right sizeError)
       case actEnv of
         Left (mkEnv, vaultVar, cleanUp) ->
             do mRespState <-
                    registryLift (applyAction req mkEnv allActions)
                    `catch` \(_ :: SizeException) ->
                        return (Just sizeError)
                    `catch` \(e :: SomeException) ->
                        do putStrLn $ "Spock Error while handling " ++ show (Wai.pathInfo req) ++ ": " ++ show e
                           return $ Just serverError
                cleanUp
                case mRespState of
                  Just respState ->
                      respond $ respStateToResponse respState
                  Nothing ->
                      do newVault <- atomically $ readTVar vaultVar
                         let req' = req { Wai.vault = V.union newVault (Wai.vault req) }
                         coreApp req' respond
         Right respState ->
             respond $ respStateToResponse respState

data SizeException
    = SizeException
    deriving (Show, Typeable)

instance Exception SizeException

requestSizeCheck :: Word64 -> Wai.Request -> IO Wai.Request
requestSizeCheck maxSize req =
    do currentSize <- newIORef 0
       return $ req
                  { Wai.requestBody =
                        do bs <- Wai.requestBody req
                           total <-
                               atomicModifyIORef currentSize $ \sz ->
                               let !nextSize = sz + fromIntegral (BS.length bs)
                               in (nextSize, nextSize)
                           if total > maxSize
                           then throwIO SizeException
                           else return bs
                  }


buildMiddleware :: forall m r. (MonadIO m, AbstractRouter r, RouteAppliedAction r ~ ActionT m ())
         => Maybe Word64
         -> r
         -> (forall a. m a -> IO a)
         -> SpockAllT r m ()
         -> IO Wai.Middleware
buildMiddleware mLimit registryIf registryLift spockActions =
    do (_, getMatchingRoutes, middlewares) <-
           registryLift $ runRegistry registryIf spockActions
       let spockMiddleware = foldl (.) id middlewares
           app :: Wai.Application -> Wai.Application
           app coreApp req respond =
            case parseMethod $ Wai.requestMethod req of
              Left _ ->
                  respond invalidReq
              Right stdMethod ->
                  do let allActions = getMatchingRoutes stdMethod (Wai.pathInfo req)
                     runResourceT $ withInternalState $ \st ->
                         handleRequest stdMethod mLimit registryLift allActions st coreApp req respond
       return $ spockMiddleware . app
