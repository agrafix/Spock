{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Web.Spock.Internal.Wire where

import Control.Applicative
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.RWS.Strict
import Control.Monad.Error
import Control.Monad.Reader.Class ()
import Control.Monad.Trans.Resource
import Data.Hashable
import Data.Maybe
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
   { uf_name :: T.Text
   , uf_contentType :: T.Text
   , uf_tempLocation :: FilePath
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

data ResponseBody
   = ResponseFile FilePath
   | ResponseLBS BSL.ByteString
   | ResponseRedirect T.Text
   deriving (Show, Eq)

data ResponseState
   = ResponseState
   { rs_responseHeaders :: [(T.Text, T.Text)]
   , rs_status :: Status
   , rs_responseBody :: ResponseBody
   } deriving (Show, Eq)

data ActionInterupt
    = ActionRedirect T.Text
    | ActionTryNext
    | ActionError String
    | ActionDone
    | ActionMiddlewarePass
    deriving (Show)

instance Error ActionInterupt where
    noMsg = ActionError "Unkown Internal Action Error"
    strMsg = ActionError

newtype ActionT m a
    = ActionT { runActionT :: ErrorT ActionInterupt (RWST RequestInfo () ResponseState m) a }
      deriving (Monad, Functor, Applicative, MonadIO, MonadReader RequestInfo, MonadState ResponseState, MonadError ActionInterupt)

instance MonadTrans ActionT where
    lift = ActionT . lift . lift

respStateToResponse :: ResponseState -> Wai.Response
respStateToResponse (ResponseState headers status body) =
    case body of
      ResponseFile fp ->
          Wai.responseFile status waiHeaders fp Nothing
      ResponseLBS bsl ->
          Wai.responseLBS status waiHeaders bsl
      ResponseRedirect target ->
          Wai.responseLBS status302 (("Location", T.encodeUtf8 target) : waiHeaders) BSL.empty
    where
      waiHeaders = map (\(k, v) -> (CI.mk $ T.encodeUtf8 k, T.encodeUtf8 v)) headers

errorResponse :: Status -> BSL.ByteString -> ResponseState
errorResponse s e =
    ResponseState
    { rs_responseHeaders = [("Content-Type", "text/html")]
    , rs_status = s
    , rs_responseBody =
        ResponseLBS $
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
                  runResourceT $
                  withInternalState $ \st ->
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
                             resp = errorResponse status200 ""
                             allActions = getMatchingRoutes stdMethod (Wai.pathInfo req)
                             applyAction :: [(ParamMap, ActionT m ())] -> m (Maybe ResponseState)
                             applyAction [] =
                                 return $ Just $ errorResponse status404 "404 - File not found"
                             applyAction ((captures, selectedAction) : xs) =
                                       do let env = RequestInfo req captures queryParams uploadedFiles vaultIf
                                          (r, respState, _) <-
                                              runRWST (runErrorT $ runActionT $ selectedAction) env resp
                                          case r of
                                            Left (ActionRedirect loc) ->
                                                return $ Just $ ResponseState (rs_responseHeaders respState)
                                                       status302 (ResponseRedirect loc)
                                            Left ActionTryNext ->
                                                applyAction xs
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
                         mRespState <-
                             liftIO $ (registryLift $ applyAction allActions)
                                        `catch` \(e :: SomeException) ->
                                            do putStrLn $ "Spock Error while handeling " ++ show (Wai.pathInfo req) ++ ": " ++ show e
                                               return $ Just serverError
                         case mRespState of
                           Just respState ->
                               do forM_ (HM.elems uploadedFiles) $ \uploadedFile ->
                                      do stillThere <- doesFileExist (uf_tempLocation uploadedFile)
                                         when stillThere $ liftIO $ removeFile (uf_tempLocation uploadedFile)
                                  liftIO $ respond $ respStateToResponse respState
                           Nothing ->
                               liftIO $
                               do newVault <- atomically $ readTVar vaultVar
                                  let req' = req { Wai.vault = V.union newVault (Wai.vault req) }
                                  coreApp req' respond
       return $ spockMiddleware . app
