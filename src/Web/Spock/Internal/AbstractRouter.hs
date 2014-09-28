{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Web.Spock.Internal.AbstractRouter where

import Control.Applicative
import Control.Monad.RWS.Strict
import Data.Hashable
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

class IsPath (path :: k -> *) where
    type CaptureFreePath path :: k
    combineSubcomp :: path (CaptureFreePath path) -> path a -> path a

type ParamMap = HM.HashMap CaptureVar T.Text

newtype CaptureVar
      = CaptureVar { unCaptureVar :: T.Text }
      deriving (Show, Eq, Hashable)

data AnyRouteRegistryIf (path :: k -> *) (action :: k -> *) actionM actionR reg
   = AnyRouteRegistryIf
   { rr_emptyRegistry :: reg
   , rr_rootPath :: path (CaptureFreePath path)
   , rr_defRoute :: forall as. path as -> action as -> reg -> reg
   , rr_matchRoute :: reg -> [T.Text] -> [(ParamMap, actionM actionR)]
   }

data RegistryState reqTypes reg
   = RegistryState
   { rs_registry :: HM.HashMap reqTypes reg
   }

data RegistryRead (path :: k -> *) (action :: k -> *) actionM actionR reg
   = RegistryRead
   { rr_basePath :: path (CaptureFreePath path)
   , rr_registryIf :: AnyRouteRegistryIf path action actionM actionR reg
   }

newtype RegistryT (path :: k -> *) (action :: k -> *) actionM actionR reg middleware reqTypes (m :: * -> *) a
    = RegistryT
    { runRegistryT ::
          RWST (RegistryRead path action actionM actionR reg) [middleware]
                   (RegistryState reqTypes reg)
                   m
                   a
    } deriving (Monad, Functor, Applicative, MonadIO
               , MonadReader (RegistryRead path action actionM actionR reg)
               , MonadWriter [middleware]
               , MonadState (RegistryState reqTypes reg)
               )

instance MonadTrans (RegistryT path action actionM actionR reg middleware reqTypes) where
    lift = RegistryT . lift

hookRoute :: (Monad m, Eq reqTypes, Hashable reqTypes)
          => reqTypes
          -> path as
          -> action as
          -> RegistryT path action actionM actionR reg middleware reqTypes m ()
hookRoute reqType path action =
    do parentRead <- ask
       let regIf = rr_registryIf parentRead
       modify $ \rs ->
           rs { rs_registry =
                    let reg = fromMaybe (rr_emptyRegistry regIf) (HM.lookup reqType (rs_registry rs))
                        reg' = (rr_defRoute regIf) path action reg
                    in HM.insert reqType reg' (rs_registry rs)
              }

middleware :: Monad m
           => middleware
           -> RegistryT path action actionM actionR reg middleware reqTypes m ()
middleware x = tell [x]

subcomponent :: (Monad m, IsPath path)
             => path (CaptureFreePath path)
             -> RegistryT path action actionM actionR reg middleware reqTypes m a
             -> RegistryT path action actionM actionR reg middleware reqTypes m a
subcomponent basePath (RegistryT subReg) =
    do parentSt <- get
       parentRead <- ask
       let childRead =
               parentRead
               { rr_basePath = (rr_basePath parentRead) `combineSubcomp` basePath
               }
           childSt = parentSt
       (a, parentSt', middleware') <-
           lift $ runRWST subReg childRead childSt
       put parentSt'
       tell middleware'
       return a

runRegistry :: (Monad m, IsPath path, Hashable reqTypes, Eq reqTypes)
             => AnyRouteRegistryIf path action actionM actionR reg
             -> RegistryT path action actionM actionR reg middleware reqTypes m a
             -> m (a, reqTypes -> [T.Text] -> [(ParamMap, actionM actionR)], [middleware])
runRegistry registryIf (RegistryT rwst) =
    do (val, st, w) <- runRWST rwst initRead initSt
       return (val, handleF (rs_registry st), w)
    where
      handleF hm ty route =
          case HM.lookup ty hm of
            Nothing -> []
            Just registry ->
                (rr_matchRoute registryIf) registry route
      initRead =
          RegistryRead
          { rr_basePath = rr_rootPath registryIf
          , rr_registryIf = registryIf
          }
      initSt =
          RegistryState
          { rs_registry = HM.empty
          }
