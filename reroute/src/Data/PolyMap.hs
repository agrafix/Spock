{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}

module Data.PolyMap
  ( PolyMap, empty, rnfHelper
  , lookup, lookupApply, lookupApplyAll, lookupConcat
  , alter,  updateAll, insertWith
  , unionWith, union
  , zipWith', zipWith, zip, ap
  ) where

import Prelude hiding (lookup, zipWith, zip)
import Data.Typeable
#if MIN_VERSION_base(4,8,0)
import Control.Applicative (Alternative ((<|>)), liftA2)
#else
import Control.Applicative (Applicative (..), Alternative ((<|>)), liftA2)
import Data.Monoid (Monoid (..))
#endif
import GHC.Exts (Constraint)

data PolyMap (c :: * -> Constraint) (f :: * -> *) (a :: *) where
  PMNil :: PolyMap c f a
  PMCons :: (Typeable p, c p) => f (p -> a) -> PolyMap c f a -> PolyMap c f a

instance Functor f => Functor (PolyMap c f) where
  fmap _ PMNil = PMNil
  fmap f (PMCons v pm) = PMCons (fmap ((.) f) v) (fmap f pm)

instance Alternative f => Monoid (PolyMap c f a) where
  mempty = empty
  mappend = union

empty :: PolyMap c f a
empty = PMNil

rnfHelper :: (forall p. c p => f (p -> a) -> ()) -> PolyMap c f a -> ()
rnfHelper _ PMNil = ()
rnfHelper h (PMCons v pm) = h v `seq` rnfHelper h pm

lookup ::
  Typeable p
  => PolyMap c f a
  -> Maybe (f (p -> a))
lookup PMNil = Nothing
lookup (PMCons w polyMap') =
  case gcast1 w of
    Just w' -> Just w'
    Nothing -> lookup polyMap'

lookupApply ::
  (Typeable p, Functor f)
  => p
  -> PolyMap c f a
  -> Maybe (f a)
lookupApply _ PMNil = Nothing
lookupApply p (PMCons w polyMap') =
  case gcast1 w of
    Just w' -> Just (fmap ($ p) w')
    Nothing -> lookupApply p polyMap'

lookupApplyAll ::
  Functor f
  => (forall p. c p => Maybe p)
  -> PolyMap c f a
  -> [f a]
lookupApplyAll maybeGet polyMap =
  case polyMap of
    PMNil -> []
    PMCons w polyMap' ->
      let rest = lookupApplyAll maybeGet polyMap'
      in case maybeGet of
           Nothing -> rest
           Just p  -> (fmap ($ p) w) : rest

lookupConcat ::
  (Monoid m, Functor f)
  => (forall p. c p => Maybe p)
  -> (forall p. c p => p -> f (p -> a) -> m)
  -> PolyMap c f a
  -> m
lookupConcat maybeGet comp polyMap =
  case polyMap of
    PMNil -> mempty
    PMCons w polyMap' ->
      let rest = lookupConcat maybeGet comp polyMap'
      in case maybeGet of
           Nothing -> rest
           Just p  -> comp p w `mappend` rest


maybeInsertHere ::
  (c p, Typeable p)
  => Maybe (f (p -> a))
  -> PolyMap c f a
  -> PolyMap c f a
maybeInsertHere = maybe id PMCons

alter ::
  (Typeable p, c p)
  => (Maybe (f (p -> a)) -> Maybe (f (p -> a)))
  -> PolyMap c f a
  -> PolyMap c f a
alter (g :: Maybe (f (p -> a)) -> Maybe (f (p -> a))) polyMap =
  case polyMap of
    PMNil -> insertHere PMNil
    PMCons (w :: f (q -> a)) polyMap' ->
      case gcast1 w of
        Just w' ->
          case g (Just w') of
            Just w'' -> PMCons w'' polyMap'
            Nothing -> polyMap'
        Nothing ->
          if typeOf (undefined :: p) < typeOf (undefined :: q)
          then insertHere polyMap
          else PMCons w (alter g polyMap')
  where insertHere = maybe id PMCons (g Nothing)

updateAll ::
     (forall p. c p => f (p -> a) -> g (p -> b))
  -> PolyMap c f a
  -> PolyMap c g b
updateAll _ PMNil = PMNil
updateAll f (PMCons v pm) = PMCons (f v) (updateAll f pm)

insertWith ::
  (Typeable p, c p)
  => (f (p -> a) -> f (p -> a) -> f (p -> a))
  -> f (p -> a)
  -> PolyMap c f a
  -> PolyMap c f a
insertWith combine val = alter (Just . maybe val (combine val))

unionWith ::
     (forall p. c p => f (p -> a) -> f (p -> a) -> f (p -> a))
  -> PolyMap c f a
  -> PolyMap c f a
  -> PolyMap c f a
unionWith _ PMNil pm2 = pm2
unionWith _ pm1 PMNil = pm1
unionWith combine pm1@(PMCons (v :: f (p -> a)) pm1')
                  pm2@(PMCons (w :: f (q -> a)) pm2') =
  case gcast1 v of
    Just v' -> PMCons (combine v' w) (unionWith combine pm1' pm2')
    Nothing ->
        if typeOf (undefined :: p) < typeOf (undefined :: q)
        then PMCons v (unionWith combine pm1' pm2)
        else PMCons w (unionWith combine pm1 pm2')

union ::
  Alternative f
  => PolyMap c f a
  -> PolyMap c f a
  -> PolyMap c f a
union = unionWith (<|>)

zipWith' ::
     (forall p. c p => Maybe (f (p -> a)) -> Maybe (f (p -> b)) -> Maybe (f (p -> d)))
  -> PolyMap c f a
  -> PolyMap c f b
  -> PolyMap c f d
zipWith' f = go
  where
    go PMNil PMNil = PMNil
    go PMNil (PMCons w pm2') = maybeInsertHere (f Nothing (Just w)) (go PMNil pm2')
    go (PMCons v pm1') PMNil = maybeInsertHere (f (Just v) Nothing) (go pm1' PMNil)
    go pm1@(PMCons (v :: f (p -> a)) pm1') pm2@(PMCons (w :: f (q -> b)) pm2') =
      case gcast1 v of
        Just v' -> maybeInsertHere (f (Just v') (Just w)) (go pm1 pm2)
        Nothing ->
          if typeOf (undefined :: p) < typeOf (undefined :: q)
          then maybeInsertHere (f (Just v) Nothing) (go pm1' pm2)
          else maybeInsertHere (f Nothing (Just w)) (go pm1 pm2')

zipWith ::
  Applicative f
  => (a -> b -> d)
  -> PolyMap c f a
  -> PolyMap c f b
  -> PolyMap c f d
zipWith f = zipWith' $ liftA2 $ liftA2 $ \a b p -> f (a p) (b p)

zip ::
  Applicative f
  => PolyMap c f a
  -> PolyMap c f b
  -> PolyMap c f (a, b)
zip = zipWith (,)

ap ::
  Applicative f
  => PolyMap c f (a -> b)
  -> PolyMap c f a
  -> PolyMap c f b
ap = zipWith ($)
