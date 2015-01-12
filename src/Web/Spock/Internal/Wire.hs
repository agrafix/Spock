{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Web.Spock.Internal.Wire where

import Control.Applicative
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.RWS.Strict
import Control.Monad.Except
import Control.Monad.Reader.Class ()
import Control.Monad.Trans.Resource
import Data.Hashable
import Data.Maybe
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

data RequestInfo
   = RequestInfo
   { ri_request :: Wai.Request
   , ri_params :: HM.HashMap CaptureVar T.Text
   , ri_queryParams :: [(T.Text, T.Text)]
   , ri_files :: HM.HashMap T.Text UploadedFile
   , ri_vaultIf :: VaultIf
   }

newtype ResponseBody = ResponseBody (Status -> ResponseHeaders -> Wai.Response)

data ResponseState
   = ResponseState
   { rs_responseHeaders :: !(HM.HashMap (CI.CI BS.ByteString) BS.ByteString)
   , rs_status :: !Status
   , rs_responseBody :: !ResponseBody
   }

data ActionInterupt
    = ActionRedirect !T.Text
    | ActionTryNext
    | ActionError String
    | ActionDone
    | ActionMiddlewarePass
    deriving (Show)


newtype ActionT m a
    = ActionT { runActionT :: ExceptT ActionInterupt (RWST RequestInfo () ResponseState m) a }
      deriving (Monad, Functor, Applicative, MonadIO, MonadReader RequestInfo, MonadState ResponseState, MonadError ActionInterupt)

instance MonadTrans ActionT where
    lift = ActionT . lift . lift

respStateToResponse :: ResponseState -> Wai.Response
respStateToResponse (ResponseState headers status (ResponseBody body)) =
    body status $ HM.toList headers

errorResponse :: Status -> BSL.ByteString -> ResponseState
errorResponse s e =
    ResponseState
    { rs_responseHeaders =
          HM.singleton "Content-Type" "text/html"
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

notFound :: Wai.Response
notFound =
    respStateToResponse $ errorResponse status404 "404 - File not found"

invalidReq :: Wai.Response
invalidReq =
    respStateToResponse $ errorResponse status400 "400 - Bad request"

serverError :: ResponseState
serverError =
    errorResponse status500 "500 - Internal Server Error!"

type SpockAllT r m a =
    RegistryT r Wai.Middleware StdMethod m a

middlewareToApp :: Wai.Middleware
                -> Wai.Application
middlewareToApp mw =
    mw fallbackApp
    where
      fallbackApp :: Wai.Application
      fallbackApp _ respond =
          respond $ notFound

makeActionEnvironment :: InternalState -> Wai.Request -> IO (ParamMap -> RequestInfo, TVar V.Vault, IO ())
makeActionEnvironment st req =
    do (bodyParams, bodyFiles) <- P.parseRequestBody (P.tempFileBackEnd st) req
       vaultVar <- liftIO $ newTVarIO (Wai.vault req)
       let vaultIf =
               VaultIf
               { vi_modifyVault = \modF -> atomically $ modifyTVar' vaultVar modF
               , vi_lookupKey = \k -> V.lookup k <$> (atomically $ readTVar vaultVar)
               }
           uploadedFiles =
               HM.fromList $
                 map (\(k, fileInfo) ->
                          ( T.decodeUtf8 k
                          , UploadedFile (T.decodeUtf8 $ P.fileName fileInfo) (T.decodeUtf8 $ P.fileContentType fileInfo) (P.fileContent fileInfo)
                          )
                     ) bodyFiles
           postParams =
               map (\(k, v) -> (T.decodeUtf8 k, T.decodeUtf8 v)) bodyParams
           getParams =
               map (\(k, mV) -> (T.decodeUtf8 k, T.decodeUtf8 $ fromMaybe BS.empty mV)) $ Wai.queryString req
           queryParams = postParams ++ getParams
       return ( \params ->
                    RequestInfo
                    { ri_request = req
                    , ri_params = params
                    , ri_queryParams = queryParams
                    , ri_files = uploadedFiles
                    , ri_vaultIf = vaultIf
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
            -> (ParamMap -> RequestInfo)
            -> [(ParamMap, ActionT m ())]
            -> m (Maybe ResponseState)
applyAction _ _ [] =
    return $ Just $ errorResponse status404 "404 - File not found"
applyAction req mkEnv ((captures, selectedAction) : xs) =
    do let env = mkEnv captures
           defResp = errorResponse status200 ""
       (r, respState, _) <-
           runRWST (runExceptT $ runActionT $ selectedAction) env defResp
       case r of
         Left (ActionRedirect loc) ->
             return $ Just $ ResponseState (rs_responseHeaders respState) status302 $ ResponseBody $
                 \status headers -> Wai.responseLBS status (("Location", T.encodeUtf8 loc) : headers) BSL.empty
         Left ActionTryNext ->
             applyAction req mkEnv xs
         Left (ActionError errorMsg) ->
             do liftIO $ putStrLn $ "Spock Error while handeling "
                             ++ show (Wai.pathInfo req) ++ ": " ++ errorMsg
                return $ Just serverError
         Left ActionDone ->
             return $ Just respState
         Left ActionMiddlewarePass ->
             return Nothing
         Right () ->
             return $ Just respState

handleRequest :: MonadIO m => (forall a. m a -> IO a)
              -> [(ParamMap, ActionT m ())]
              -> InternalState
              -> Wai.Application -> Wai.Application
handleRequest registryLift allActions st coreApp req respond =
    do (mkEnv, vaultVar, cleanUp) <- makeActionEnvironment st req
       mRespState <-
           (registryLift $ applyAction req mkEnv allActions)
           `catch` \(e :: SomeException) ->
              do putStrLn $ "Spock Error while handeling " ++ show (Wai.pathInfo req) ++ ": " ++ show e
                 return $ Just serverError
       cleanUp
       case mRespState of
         Just respState ->
             respond $ respStateToResponse respState
         Nothing ->
             do newVault <- atomically $ readTVar vaultVar
                let req' = req { Wai.vault = V.union newVault (Wai.vault req) }
                coreApp req' respond

buildMiddleware :: forall m r. (MonadIO m, AbstractRouter r, RouteAppliedAction r ~ ActionT m ())
         => r
         -> (forall a. m a -> IO a)
         -> SpockAllT r m ()
         -> IO Wai.Middleware
buildMiddleware registryIf registryLift spockActions =
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
                         handleRequest registryLift allActions st coreApp req respond
       return $ spockMiddleware . app
