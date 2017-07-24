{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Web.Routing.Router where

import Web.Routing.SafeRouting

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif
import Control.Monad.RWS.Strict
import Data.Hashable
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

newtype RegistryT n b middleware reqTypes (m :: * -> *) a
    = RegistryT
    { runRegistryT :: RWST (PathInternal '[]) [middleware] (RegistryState n b reqTypes) m a
    }
    deriving (Monad, Functor, Applicative, MonadIO
               , MonadReader (PathInternal '[])
               , MonadWriter [middleware]
               , MonadState (RegistryState n b reqTypes)
               , MonadTrans
               )

data RegistryState n b reqTypes
   = RegistryState
   { rs_registry :: HM.HashMap reqTypes (Registry n b)
   }

hookAny :: (Monad m, Eq reqTypes, Hashable reqTypes)
        => reqTypes
        -> ([T.Text] -> n b)
        -> RegistryT n b middleware reqTypes m ()
hookAny reqType action =
    modify $ \rs ->
        rs
        { rs_registry =
                let reg = fromMaybe emptyRegistry (HM.lookup reqType (rs_registry rs))
                in HM.insert reqType (fallbackRoute action reg) (rs_registry rs)
        }

hookRoute :: (Monad m, Eq reqTypes, Hashable reqTypes)
          => reqTypes
          -> PathInternal as
          -> HVectElim' (n b) as
          -> RegistryT n b middleware reqTypes m ()
hookRoute reqType path action =
    do basePath <- ask
       modify $ \rs ->
           rs { rs_registry =
                    let reg = fromMaybe emptyRegistry (HM.lookup reqType (rs_registry rs))
                        reg' = defRoute (basePath </!> path) action reg
                    in HM.insert reqType reg' (rs_registry rs)
              }

middleware :: Monad m
           => middleware
           -> RegistryT n b middleware reqTypes m ()
middleware x = tell [x]

swapMonad ::
    Monad m
    => (forall b. n b -> m b)
    -> RegistryT x y middleware reqTypes n a
    -> RegistryT x y middleware reqTypes m a
swapMonad liftLower (RegistryT subReg) =
    do parentSt <- get
       basePath <- ask
       (a, parentSt', middleware') <-
           lift $ liftLower $ runRWST subReg basePath parentSt
       put parentSt'
       tell middleware'
       return a

runRegistry :: (Monad m, Hashable reqTypes, Eq reqTypes)
            => RegistryT n b middleware reqTypes m a
            -> m (a, reqTypes -> [T.Text] -> [n b], [middleware])
runRegistry (RegistryT rwst) =
    do (val, st, w) <- runRWST rwst PI_Empty initSt
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
