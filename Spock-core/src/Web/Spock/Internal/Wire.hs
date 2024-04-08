{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

module Web.Spock.Internal.Wire where

import Control.Applicative
import Control.Arrow ((***))
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Base
#if MIN_VERSION_mtl(2,2,0)
import Control.Monad.Except
#else
import Control.Monad.Error
#endif

#if MIN_VERSION_base(4,6,0)
import Prelude
#else
import Prelude hiding (catch)
#endif

import qualified Control.Monad.Morph as MM
import Control.Monad.RWS.Strict hiding ((<>))
import Control.Monad.Reader.Class ()
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.SuperBuffer as SB
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.IORef
import Data.Maybe
import Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Typeable
import qualified Data.Vault.Lazy as V
import Data.Word
import GHC.Generics
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as P
import System.IO
import Web.Routing.Router

newtype HttpMethod = HttpMethod {unHttpMethod :: StdMethod}
  deriving (Show, Eq, Bounded, Enum, Generic)

instance Hashable HttpMethod where
  hashWithSalt = hashUsing (fromEnum . unHttpMethod)

-- | The 'SpockMethod' allows safe use of http verbs via the 'MethodStandard'
-- constructor and 'StdMethod', and custom verbs via the 'MethodCustom' constructor.
data SpockMethod
  = -- | Standard HTTP Verbs from 'StdMethod'
    MethodStandard !HttpMethod
  | -- | Custom HTTP Verbs using 'T.Text'
    MethodCustom !T.Text
  | -- | Match any HTTP verb
    MethodAny
  deriving (Eq, Generic)

instance Hashable SpockMethod

data UploadedFile = UploadedFile
  { uf_name :: !T.Text,
    uf_contentType :: !T.Text,
    uf_tempLocation :: !FilePath
  }
  deriving (Show)

data VaultIf = VaultIf
  { vi_modifyVault :: (V.Vault -> V.Vault) -> IO (),
    vi_lookupKey :: forall a. V.Key a -> IO (Maybe a)
  }

data CacheVar v = forall r.
  CacheVar
  { cv_lock :: !(MVar ()),
    cv_makeVal :: !(IO r),
    cv_value :: !(IORef (Maybe r)),
    cv_read :: r -> v
  }

instance Functor CacheVar where
  fmap f (CacheVar lock makeVal valRef readV) =
    CacheVar
      { cv_lock = lock,
        cv_makeVal = makeVal,
        cv_value = valRef,
        cv_read = f . readV
      }

newCacheVar :: IO v -> IO (CacheVar v)
newCacheVar makeVal =
  do
    lock <- newEmptyMVar
    valueR <- newIORef Nothing
    return (CacheVar lock makeVal valueR id)

loadCacheVarOpt :: CacheVar v -> IO (Maybe v)
loadCacheVarOpt (CacheVar lock _ valRef readV) =
  bracket_ (putMVar lock ()) (takeMVar lock) $
    fmap readV <$> readIORef valRef

loadCacheVar :: CacheVar v -> IO v
loadCacheVar (CacheVar lock makeVal valRef readV) =
  bracket_ (putMVar lock ()) (takeMVar lock) $
    do
      val <- readIORef valRef
      case val of
        Just v -> return (readV v)
        Nothing ->
          do
            v <- makeVal
            writeIORef valRef (Just v)
            return (readV v)

data RequestBody = RequestBody
  { rb_value :: CacheVar BS.ByteString,
    rb_postParams :: CacheVar [(T.Text, T.Text)],
    rb_files :: CacheVar (HM.HashMap T.Text [UploadedFile])
  }

data RequestInfo ctx = RequestInfo
  { ri_method :: !SpockMethod,
    ri_request :: !Wai.Request,
    ri_getParams :: ![(T.Text, T.Text)],
    ri_reqBody :: !RequestBody,
    ri_vaultIf :: !VaultIf,
    ri_context :: !ctx
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
  HM.fromList $
    flip map allHeaders $ \mh ->
      (multiHeaderCI mh, mh)
  where
    -- this is a nasty hack until we know more about the origin of uncaught
    -- exception: ErrorCall (toEnum{MultiHeader}: tag (-12565) is outside of
    -- enumeration's range (0,12)) see:
    -- https://ghc.haskell.org/trac/ghc/ticket/10792 and
    -- https://github.com/agrafix/Spock/issues/44
    allHeaders =
      [ MultiHeaderCacheControl,
        MultiHeaderConnection,
        MultiHeaderContentEncoding,
        MultiHeaderContentLanguage,
        MultiHeaderPragma,
        MultiHeaderProxyAuthenticate,
        MultiHeaderTrailer,
        MultiHeaderTransferEncoding,
        MultiHeaderUpgrade,
        MultiHeaderVia,
        MultiHeaderWarning,
        MultiHeaderWWWAuth,
        MultiHeaderSetCookie
      ]

data ResponseVal
  = ResponseValState !ResponseState
  | ResponseHandler !(IO Wai.Application)

data ResponseState = ResponseState
  { rs_responseHeaders :: !(HM.HashMap (CI.CI BS.ByteString) BS.ByteString),
    rs_multiResponseHeaders :: !(HM.HashMap MultiHeader [BS.ByteString]),
    rs_status :: !Status,
    rs_responseBody :: !ResponseBody
  }

data ActionInterupt
  = ActionRedirect !T.Text
  | ActionTryNext
  | ActionError String
  | ActionDone
  | ActionMiddlewarePass
  | ActionMiddleware !(IO Wai.Middleware)
  | ActionApplication !(IO Wai.Application)
  deriving (Typeable)

instance Semigroup ActionInterupt where
  _ <> a = a

instance Monoid ActionInterupt where
  mempty = ActionDone
  mappend = (<>)

#if MIN_VERSION_mtl(2,2,0)
type ErrorT = ExceptT

runErrorT :: ExceptT e m a -> m (Either e a)
runErrorT = runExceptT

toErrorT :: m (Either e a) -> ErrorT e m a
toErrorT = ExceptT
#else
toErrorT :: m (Either e a) -> ErrorT e m a
toErrorT = ErrorT

instance Error ActionInterupt where
    noMsg = ActionError "Unkown Internal Action Error"
    strMsg = ActionError
#endif

type ActionT = ActionCtxT ()

newtype ActionCtxT ctx m a = ActionCtxT
  {runActionCtxT :: ErrorT ActionInterupt (RWST (RequestInfo ctx) () ResponseState m) a}
  deriving
    ( Monad,
      Functor,
      Applicative,
      Alternative,
      MonadIO,
      MonadReader (RequestInfo ctx),
      MonadState ResponseState,
      MonadError ActionInterupt
    )

instance MonadTrans (ActionCtxT ctx) where
  lift = ActionCtxT . lift . lift

instance MM.MFunctor (ActionCtxT ctx) where
  hoist f m = ActionCtxT (MM.hoist (MM.hoist f) (runActionCtxT m))

instance MonadTransControl (ActionCtxT ctx) where
  type StT (ActionCtxT ctx) a = (Either ActionInterupt a, ResponseState, ())
  liftWith f =
    ActionCtxT . toErrorT . RWST $ \requestInfo responseState ->
      fmap
        (\x -> (pure x, responseState, ()))
        (f $ \(ActionCtxT lala) -> runRWST (runErrorT lala) requestInfo responseState)
  restoreT mSt = ActionCtxT . toErrorT $ RWST (\_ _ -> mSt)

instance MonadBase b m => MonadBase b (ActionCtxT ctx m) where
  liftBase = liftBaseDefault

instance MonadBaseControl b m => MonadBaseControl b (ActionCtxT ctx m) where
  type StM (ActionCtxT ctx m) a = ComposeSt (ActionCtxT ctx) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

data SpockConfigInternal = SpockConfigInternal
  { sci_maxRequestSize :: Maybe Word64,
    sci_errorHandler :: Status -> IO Wai.Application,
    sci_logError :: T.Text -> IO ()
  }

defaultSpockConfigInternal :: SpockConfigInternal
defaultSpockConfigInternal =
  SpockConfigInternal Nothing defaultErrorHandler (T.hPutStrLn stderr)
  where
    defaultErrorHandler status = return $ \_ respond ->
      do
        let errorMessage =
              "Error handler failed with status code " ++ show (statusCode status)
        respond $ Wai.responseLBS status500 [] $ BSLC.pack errorMessage

respStateToResponse :: ResponseVal -> Wai.Response
respStateToResponse (ResponseValState (ResponseState headers multiHeaders status (ResponseBody body))) =
  let mkMultiHeader (k, vals) =
        let kCi = multiHeaderCI k
         in map (\v -> (kCi, v)) vals
      outHeaders =
        HM.toList headers
          ++ (concatMap mkMultiHeader $ HM.toList multiHeaders)
   in body status outHeaders
respStateToResponse _ = error "ResponseState expected"

errorResponse :: Status -> BSL.ByteString -> ResponseVal
errorResponse s e =
  ResponseValState
    ResponseState
      { rs_responseHeaders =
          HM.singleton "Content-Type" "text/html",
        rs_multiResponseHeaders =
          HM.empty,
        rs_status = s,
        rs_responseBody = ResponseBody $ \status headers ->
          Wai.responseLBS status headers $
            BSL.concat
              [ "<html><head><title>",
                e,
                "</title></head><body><h1>",
                e,
                "</h1></body></html>"
              ]
      }

defResponse :: ResponseState
defResponse =
  ResponseState
    { rs_responseHeaders =
        HM.empty,
      rs_multiResponseHeaders =
        HM.empty,
      rs_status = status200,
      rs_responseBody = ResponseBody $ \status headers ->
        Wai.responseLBS status headers $
          BSL.empty
    }

type SpockAllT n m a = RegistryT (ActionT n) () Wai.Middleware SpockMethod m a

middlewareToApp ::
  Wai.Middleware ->
  Wai.Application
middlewareToApp mw =
  mw fallbackApp
  where
    fallbackApp :: Wai.Application
    fallbackApp _ respond = respond notFound
    notFound = respStateToResponse $ errorResponse status404 "404 - File not found"

makeActionEnvironment ::
  InternalState -> SpockMethod -> Wai.Request -> IO (RequestInfo (), TVar V.Vault)
makeActionEnvironment st stdMethod req =
  do
    vaultVar <- liftIO $ newTVarIO (Wai.vault req)
    let vaultIf =
          VaultIf
            { vi_modifyVault = atomically . modifyTVar' vaultVar,
              vi_lookupKey = \k -> V.lookup k <$> atomically (readTVar vaultVar)
            }
        getParams =
          map (\(k, mV) -> (T.decodeUtf8 k, T.decodeUtf8 $ fromMaybe BS.empty mV)) $ Wai.queryString req
    rbValue <-
      newCacheVar $
        do
          let parseBody = Wai.getRequestBodyChunk req
              bodyLength = Wai.requestBodyLength req
              buffStart =
                case bodyLength of
                  Wai.ChunkedBody -> 1024
                  Wai.KnownLength x -> fromIntegral x
          SB.withBuffer buffStart $ \sb ->
            do
              let loop =
                    do
                      b <- parseBody
                      if BS.null b then pure () else (SB.appendBuffer sb b >> loop)
              loop
    bodyTuple <-
      newCacheVar $
        case P.getRequestBodyType req of
          Nothing -> return ([], HM.empty)
          Just rbt ->
            do
              bodyBs <- loadCacheVar rbValue
              bodyRef <- newIORef (Just bodyBs)
              let loader =
                    do
                      mb <- readIORef bodyRef
                      case mb of
                        Just b -> writeIORef bodyRef Nothing >> pure b
                        Nothing -> pure BS.empty
              (bodyParams, bodyFiles) <-
                P.sinkRequestBody (P.tempFileBackEnd st) rbt loader
              let uploadedFiles =
                    HM.fromListWith (<>) $
                      flip map bodyFiles $ \(k, fileInfo) ->
                        ( T.decodeUtf8 k,
                          [UploadedFile
                            (T.decodeUtf8 $ P.fileName fileInfo)
                            (T.decodeUtf8 $ P.fileContentType fileInfo)
                            (P.fileContent fileInfo)]
                        )
                  postParams =
                    map (T.decodeUtf8 *** T.decodeUtf8) bodyParams
              return (postParams, uploadedFiles)
    let reqBody =
          RequestBody
            { rb_value = rbValue,
              rb_files = fmap snd bodyTuple,
              rb_postParams = fmap fst bodyTuple
            }
    return
      ( RequestInfo
          { ri_method = stdMethod,
            ri_request = req,
            ri_getParams = getParams,
            ri_reqBody = reqBody,
            ri_vaultIf = vaultIf,
            ri_context = ()
          },
        vaultVar
      )

applyAction ::
  MonadIO m =>
  SpockConfigInternal ->
  Wai.Request ->
  RequestInfo () ->
  [ActionT m ()] ->
  m (Maybe ResponseVal)
applyAction config _ _ [] =
  return $ Just $ getErrorHandler config status404
applyAction config req env (selectedAction : xs) =
  do
    (r, respState, _) <-
      runRWST (runErrorT $ runActionCtxT selectedAction) env defResponse
    case r of
      Left (ActionRedirect loc) ->
        return $
          Just $
            ResponseValState $
              respState
                { rs_status = status302,
                  rs_responseBody =
                    ResponseBody $ \status headers ->
                      Wai.responseLBS status (("Location", T.encodeUtf8 loc) : headers) BSL.empty
                }
      Left ActionTryNext ->
        applyAction config req env xs
      Left (ActionError errorMsg) ->
        do
          liftIO $
            sci_logError config $
              T.pack $
                "Spock Error while handling "
                  ++ show (Wai.pathInfo req)
                  ++ ": "
                  ++ errorMsg
          return $ Just $ getErrorHandler config status500
      Left ActionDone ->
        return $ Just (ResponseValState respState)
      Left ActionMiddlewarePass ->
        return Nothing
      Left (ActionApplication app) ->
        return $ Just (ResponseHandler app)
      Left (ActionMiddleware getMiddleware) ->
        return $
          Just $
            ResponseHandler $
              do
                errHandler <- sci_errorHandler config status404
                mw <- getMiddleware
                return $ mw errHandler
      Right () ->
        return $ Just (ResponseValState respState)

handleRequest ::
  MonadIO m =>
  SpockConfigInternal ->
  SpockMethod ->
  (forall a. m a -> IO a) ->
  [ActionT m ()] ->
  InternalState ->
  Wai.Application ->
  Wai.Application
handleRequest config stdMethod registryLift allActions st coreApp req respond =
  do
    reqGo <-
      case sci_maxRequestSize config of
        Nothing -> return req
        Just lim -> requestSizeCheck lim req
    handleRequest' config stdMethod registryLift allActions st coreApp reqGo respond

handleRequest' ::
  MonadIO m =>
  SpockConfigInternal ->
  SpockMethod ->
  (forall a. m a -> IO a) ->
  [ActionT m ()] ->
  InternalState ->
  Wai.Application ->
  Wai.Application
handleRequest' config stdMethod registryLift allActions st coreApp req respond =
  do
    actEnv <-
      (Left <$> makeActionEnvironment st stdMethod req)
        `catch` \(_ :: SizeException) ->
          return (Right $ getErrorHandler config status413)
    case actEnv of
      Left (mkEnv, vaultVar) ->
        do
          mRespState <-
            registryLift (applyAction config req mkEnv allActions)
              `catches` [ Handler $ \(_ :: SizeException) ->
                            return (Just $ getErrorHandler config status413),
                          Handler $ \(e :: SomeException) ->
                            do
                              sci_logError config $
                                T.pack $
                                  "Spock Error while handling " ++ show (Wai.pathInfo req)
                                    ++ ": "
                                    ++ show e
                              return $ Just $ getErrorHandler config status500
                        ]
          case mRespState of
            Just (ResponseHandler responseHandler) ->
              responseHandler >>= \app -> app req respond
            Just respState ->
              respond $ respStateToResponse respState
            Nothing ->
              do
                newVault <- atomically $ readTVar vaultVar
                let req' = req {Wai.vault = V.union newVault (Wai.vault req)}
                coreApp req' respond
      Right respState ->
        respond $ respStateToResponse respState

getErrorHandler :: SpockConfigInternal -> Status -> ResponseVal
getErrorHandler config = ResponseHandler . sci_errorHandler config

data SizeException
  = SizeException
  deriving (Show, Typeable)

instance Exception SizeException

requestSizeCheck :: Word64 -> Wai.Request -> IO Wai.Request
requestSizeCheck maxSize req =
  do
    currentSize <- newIORef 0
    return $
      req
        { Wai.requestBody =
            do
              bs <- Wai.getRequestBodyChunk req
              total <-
                atomicModifyIORef currentSize $ \sz ->
                  let !nextSize = sz + fromIntegral (BS.length bs)
                   in (nextSize, nextSize)
              if total > maxSize
                then throwIO SizeException
                else return bs
        }

buildMiddleware ::
  forall m.
  (MonadIO m) =>
  SpockConfigInternal ->
  (forall a. m a -> IO a) ->
  SpockAllT m m () ->
  IO Wai.Middleware
buildMiddleware config registryLift spockActions =
  do
    (_, getMatchingRoutes, middlewares) <-
      registryLift $ runRegistry spockActions
    let spockMiddleware = foldl (.) id middlewares
        app :: Wai.Application -> Wai.Application
        app coreApp req respond =
          withSpockMethod (Wai.requestMethod req) $
            \method ->
              do
                let allActions = getMatchingRoutes method (Wai.pathInfo req)
                runResourceT $
                  withInternalState $ \st ->
                    handleRequest config method registryLift allActions st coreApp req respond
    return $ spockMiddleware . app

withSpockMethod :: forall t. Method -> (SpockMethod -> t) -> t
withSpockMethod method cnt =
  case parseMethod method of
    Left _ ->
      cnt (MethodCustom $ T.decodeUtf8 method)
    Right stdMethod ->
      cnt (MethodStandard $ HttpMethod stdMethod)
