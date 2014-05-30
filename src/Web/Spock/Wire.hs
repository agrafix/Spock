{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Spock.Wire where

import Web.Spock.Routing

import Control.Applicative
import Control.Exception
import Control.Monad.RWS.Strict
import Control.Monad.Reader.Class ()
import Control.Monad.State.Strict
import Control.Monad.Trans.Resource
import Data.Hashable
import Data.Maybe
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as P
import System.Directory

instance Hashable StdMethod where
    hashWithSalt = hashUsing fromEnum

type SpockRoutingTree m = RoutingTree (ActionT m ())
type SpockTreeMap m = HM.HashMap StdMethod (SpockRoutingTree m)

data SpockState m
   = SpockState
   { ss_treeMap :: !(SpockTreeMap m)
   , ss_middleware :: Wai.Middleware
   }

data UploadedFile
   = UploadedFile
   { uf_name :: T.Text
   , uf_contentType :: T.Text
   , uf_tempLocation :: FilePath
   }

data RequestInfo
   = RequestInfo
   { ri_request :: Wai.Request
   , ri_params :: HM.HashMap CaptureVar T.Text
   , ri_queryParams :: [(T.Text, T.Text)]
   , ri_files :: HM.HashMap T.Text UploadedFile
   }

data ResponseBody
   = ResponseFile FilePath
   | ResponseLBS BSL.ByteString
   | ResponseRedirect T.Text

data ResponseState
   = ResponseState
   { rs_responseHeaders :: [(T.Text, T.Text)]
   , rs_status :: Status
   , rs_responseBody :: ResponseBody
   }

newtype ActionT m a
    = ActionT { runActionT :: RWST RequestInfo () ResponseState m a }
      deriving (Monad, Functor, Applicative, MonadIO, MonadTrans, MonadReader RequestInfo, MonadState ResponseState)

newtype SpockT (m :: * -> *) a
    = SpockT { runSpockT :: StateT (SpockState m) m a }
      deriving (Monad, Functor, Applicative, MonadIO, MonadState (SpockState m))

instance MonadTrans SpockT where
    lift = SpockT . lift

initState :: forall (m :: * -> *). SpockState m
initState =
    SpockState
    { ss_treeMap = HM.empty
    , ss_middleware = id
    }

respStateToResponse :: ResponseState -> Wai.Response
respStateToResponse (ResponseState headers status body) =
    case body of
      ResponseFile fp ->
          Wai.responseFile status waiHeaders fp Nothing
      ResponseLBS bsl ->
          Wai.responseLBS status waiHeaders bsl
      ResponseRedirect target ->
          Wai.responseLBS status302 [("Location", T.encodeUtf8 target)] BSL.empty
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

buildApp :: forall m. (MonadIO m)
         => (forall a. m a -> IO a)
         -> SpockT m ()
         -> IO Wai.Application
buildApp spockLift spockActions =
    do spockState <- spockLift $ execStateT (runSpockT spockActions) initState
       let app req =
            case parseMethod $ Wai.requestMethod req of
              Left _ ->
                  return invalidReq
              Right stdMethod ->
                  case HM.lookup stdMethod $ ss_treeMap spockState of
                    Just routeTree ->
                        case matchRoute' (Wai.pathInfo req) routeTree of
                          Just (captures, action) ->
                              do (bodyParams, bodyFiles) <- runResourceT $ withInternalState $ \st -> P.parseRequestBody (P.tempFileBackEnd st) req
                                 let uploadedFiles =
                                         HM.fromList $
                                         map (\(k, fileInfo) ->
                                                  ( T.decodeUtf8 k
                                                  , UploadedFile (T.decodeUtf8 $ P.fileName fileInfo) (T.decodeUtf8 $ P.fileContentType fileInfo) (P.fileContent fileInfo)
                                                  )
                                             ) bodyFiles
                                     postParams = map (\(k, v) -> (T.decodeUtf8 k, T.decodeUtf8 v)) bodyParams
                                     getParams = map (\(k, mV) -> (T.decodeUtf8 k, T.decodeUtf8 $ fromMaybe BS.empty mV)) $ Wai.queryString req
                                     queryParams = postParams ++ getParams
                                     env = RequestInfo req captures queryParams uploadedFiles
                                     resp = errorResponse status200 ""
                                 (respState, _) <-
                                     (spockLift $ execRWST (runActionT action) env resp)
                                     `catch` \(e :: SomeException) ->
                                         do putStrLn $ "Spock Error: " ++ show e
                                            return (serverError, ())
                                 forM_ (HM.elems uploadedFiles) $ \uploadedFile ->
                                     do stillThere <- doesFileExist (uf_tempLocation uploadedFile)
                                        when stillThere $ removeFile (uf_tempLocation uploadedFile)
                                 return $ respStateToResponse respState
                          Nothing ->
                              return notFound
                    Nothing ->
                        return notFound
       return $ ss_middleware spockState $ app

-- | Hook up a 'Wai.Middleware'
middleware :: MonadIO m => Wai.Middleware -> SpockT m ()
middleware mw =
    modify $ \st -> st { ss_middleware = mw . (ss_middleware st) }

-- | Define a route matching a provided 'StdMethod' and route
defRoute :: (MonadIO m) => StdMethod -> T.Text -> ActionT m () -> SpockT m ()
defRoute method route action =
    modify $ \st -> st { ss_treeMap = HM.insertWith updFun method (addToTree emptyRoutingTree) (ss_treeMap st) }
    where
      updFun _ oldTree = addToTree oldTree
      addToTree = addToRoutingTree route action
