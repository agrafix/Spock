{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Web.Routing.AbstractRouter where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif
import Control.Monad.RWS.Strict
import Data.Hashable
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Control.DeepSeq (NFData (..))

class AbstractRouter r where
    data Registry r :: *
    data RoutePath r :: [*] -> *
    type RouteAction r :: [*] -> *
    type RouteAppliedAction r
    subcompCombine :: RoutePath r '[] -> RoutePath r as -> RoutePath r as
    emptyRegistry :: Registry r
    rootPath :: RoutePath r '[]
    defRoute :: RoutePath r as -> RouteAction r as -> Registry r -> Registry r
    fallbackRoute :: ([T.Text] -> RouteAppliedAction r) -> Registry r -> Registry r
    matchRoute :: Registry r -> [T.Text] -> [(ParamMap, RouteAppliedAction r)]

type ParamMap = HM.HashMap CaptureVar T.Text

newtype CaptureVar
      = CaptureVar { unCaptureVar :: T.Text }
      deriving (Show, Eq, Hashable, NFData)

newtype RegistryT r middleware reqTypes (m :: * -> *) a
    = RegistryT { runRegistryT :: RWST (RoutePath r '[]) [middleware] (RegistryState r reqTypes) m a }
    deriving (Monad, Functor, Applicative, MonadIO
               , MonadReader (RoutePath r '[])
               , MonadWriter [middleware]
               , MonadState (RegistryState r reqTypes)
               , MonadTrans
               )

data RegistryState r reqTypes
   = RegistryState
   { rs_registry :: HM.HashMap reqTypes (Registry r)
   }

hookAny :: (Monad m, AbstractRouter r, Eq reqTypes, Hashable reqTypes)
        => reqTypes
        -> ([T.Text] -> RouteAppliedAction r)
        -> RegistryT r middleware reqTypes m ()
hookAny reqType action =
    modify $ \rs ->
        rs { rs_registry =
                 let reg = fromMaybe emptyRegistry (HM.lookup reqType (rs_registry rs))
                 in HM.insert reqType (fallbackRoute action reg) (rs_registry rs)
           }

hookRoute :: (Monad m, AbstractRouter r, Eq reqTypes, Hashable reqTypes)
          => reqTypes
          -> RoutePath r as
          -> RouteAction r as
          -> RegistryT r middleware reqTypes m ()
hookRoute reqType path action =
    do basePath <- ask
       modify $ \rs ->
           rs { rs_registry =
                    let reg = fromMaybe emptyRegistry (HM.lookup reqType (rs_registry rs))
                        reg' = defRoute (basePath `subcompCombine` path) action reg
                    in HM.insert reqType reg' (rs_registry rs)
              }

middleware :: Monad m
           => middleware
           -> RegistryT r middleware reqTypes m ()
middleware x = tell [x]

subcomponent :: (Monad m, AbstractRouter r)
             => RoutePath r '[]
             -> RegistryT r middleware reqTypes m a
             -> RegistryT r middleware reqTypes m a
subcomponent basePath (RegistryT subReg) =
    do parentSt <- get
       parentBasePath <- ask
       let childBasePath = parentBasePath `subcompCombine` basePath
           childSt = parentSt
       (a, parentSt', middleware') <-
           lift $ runRWST subReg childBasePath childSt
       put parentSt'
       tell middleware'
       return a

swapMonad ::
    (Monad n, Monad m, AbstractRouter r)
    => (forall b. n b -> m b)
    -> RegistryT r middleware reqTypes n a
    -> RegistryT r middleware reqTypes m a
swapMonad liftLower (RegistryT subReg) =
    do parentSt <- get
       basePath <- ask
       (a, parentSt', middleware') <-
           lift $ liftLower $ runRWST subReg basePath parentSt
       put parentSt'
       tell middleware'
       return a

runRegistry :: (Monad m, AbstractRouter r, Hashable reqTypes, Eq reqTypes)
            => r
            -> RegistryT r middleware reqTypes m a
            -> m (a, reqTypes -> [T.Text] -> [(ParamMap, RouteAppliedAction r)], [middleware])
runRegistry _ (RegistryT rwst) =
    do (val, st, w) <- runRWST rwst rootPath initSt
       return (val, handleF (rs_registry st), w)
    where
      handleF hm ty route =
          case HM.lookup ty hm of
            Nothing -> []
            Just registry ->
                matchRoute registry (filter (not . T.null) route)
      initSt =
          RegistryState
          { rs_registry = HM.empty
          }
