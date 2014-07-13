{-# LANGUAGE CPP #-}
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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as P

instance Hashable StdMethod where
    hashWithSalt = hashUsing fromEnum

type SpockRoutingTree m = RoutingTree (ActionT m ())
type SpockTreeMap m = HM.HashMap StdMethod (SpockRoutingTree m)

type SpockRouteMap m = HM.HashMap StdMethod (HM.HashMap T.Text (ActionT m ()))

data SpockState m
   = SpockState
   { ss_treeMap :: !(SpockRouteMap m)
   , ss_middleware :: Wai.Middleware
   , ss_spockLift :: forall a. m a -> IO a
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

type BaseRoute = T.Text

newtype ActionT m a
    = ActionT { runActionT :: RWST RequestInfo () ResponseState m a }
      deriving (Monad, Functor, Applicative, MonadIO, MonadTrans, MonadReader RequestInfo, MonadState ResponseState)

newtype SpockT (m :: * -> *) a
    = SpockT { runSpockT :: RWST BaseRoute () (SpockState m) m a }
      deriving (Monad, Functor, Applicative, MonadIO, MonadReader BaseRoute, MonadState (SpockState m))

instance MonadTrans SpockT where
    lift = SpockT . lift

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
    do let initState =
               SpockState
               { ss_treeMap = HM.empty
               , ss_middleware = id
               , ss_spockLift = spockLift
               }
       (spockState, ()) <- spockLift $ execRWST (runSpockT spockActions) "/" initState
       let routingTreeMap = buildRoutingTree (ss_treeMap spockState)
           app :: Wai.Application
           app req respond =
            case parseMethod $ Wai.requestMethod req of
              Left _ ->
                  respond invalidReq
              Right stdMethod ->
                  case HM.lookup stdMethod routingTreeMap of
                    Just routeTree ->
                        case matchRoute' (Wai.pathInfo req) routeTree of
                          Just (captures, action) ->
                              do (bodyParams, bodyFiles) <-
                                     runResourceT $
                                     withInternalState $ \st ->
                                         P.parseRequestBody (P.tempFileBackEnd st) req
                                 let uploadedFiles =
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
                                 respond $ respStateToResponse respState
                          Nothing ->
                              respond notFound
                    Nothing ->
                        respond notFound
       return $ ss_middleware spockState $ app

-- | Hook up a 'Wai.Middleware'
middleware :: MonadIO m => Wai.Middleware -> SpockT m ()
middleware mw =
    modify $ \st -> st { ss_middleware = mw . (ss_middleware st) }

-- | Define a route matching a provided 'StdMethod' and route
defRoute :: (MonadIO m) => StdMethod -> T.Text -> ActionT m () -> SpockT m ()
defRoute method route action =
    do baseRoute <- ask
       let fullRoute = baseRoute `combineRoute` route
       modify $ \st -> st { ss_treeMap = HM.insertWith HM.union method (HM.singleton fullRoute action) (ss_treeMap st) }

-- | Combine two routes, ensuring that the slashes don't get messed up
combineRoute :: T.Text -> T.Text -> T.Text
combineRoute r1 r2 =
    case T.uncons r1 of
      Nothing -> T.concat ["/", r2']
      Just ('/', _) -> T.concat [r1', r2']
      Just _ -> T.concat ["/", r1', r2']
    where
      r1' =
          if T.last r1 == '/'
          then r1
          else if T.null r2
               then r1
               else T.concat [r1, "/"]
      r2' =
          if T.null r2
          then ""
          else if T.head r2 == '/' then T.drop 1 r2 else r2

-- | Define a subcomponent
--
-- > subcomponent "/api" $
-- >    do get "/user" $ text "USER"
-- >       post "/new-user" $ text "OK!"
--
-- >>> curl http://localhost:8080/api/user
-- USER
--
subcomponent :: (MonadIO m) => T.Text -> SpockT m () -> SpockT m ()
subcomponent baseRoute defs =
    do parentState <- get
       parentRoute <- ask
       let initState =
               parentState
               { ss_treeMap = HM.empty }
       (finalState, ()) <-
           liftIO $ (ss_spockLift parentState) $
           execRWST (runSpockT defs) (parentRoute `combineRoute` baseRoute) initState
       modify $ \st ->
           st
           { ss_treeMap = HM.union (ss_treeMap st) (ss_treeMap finalState)
           , ss_middleware = (ss_middleware finalState) . (ss_middleware st)
           }

buildRoutingTree :: SpockRouteMap m -> SpockTreeMap m
buildRoutingTree routeMap =
    HM.map (\v -> foldl treeBuilder emptyRoutingTree $ HM.toList v) routeMap
    where
      treeBuilder tree (route, action) =
          addToRoutingTree route action tree
