{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Web.Routing.SafeRouting where

import qualified Data.PolyMap as PM
import Data.HVect hiding (null, length)
import qualified Data.HVect as HV
import Web.Routing.AbstractRouter

import Data.Maybe
import Data.List (foldl')
#if MIN_VERSION_base(4,8,0)
import Control.Applicative (Alternative (..))
#else
import Data.Monoid (Monoid (..))
import Control.Applicative (Applicative (..), Alternative (..))
#endif
import Data.String
import Data.Typeable (Typeable)
import Control.DeepSeq (NFData (..))
import Web.PathPieces
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

data RouteHandle m a
   = forall as. RouteHandle (Path as) (HVectElim as (m a))

newtype HVectElim' x ts = HVectElim' { flipHVectElim :: HVectElim ts x }

data SafeRouter (m :: * -> *) a = SafeRouter

instance AbstractRouter (SafeRouter m a) where
    newtype Registry (SafeRouter m a) = SafeRouterReg (PathMap (m a), [[T.Text] -> m a])
    newtype RoutePath (SafeRouter m a) xs = SafeRouterPath (Path xs)
    type RouteAction (SafeRouter m a) = HVectElim' (m a)
    type RouteAppliedAction (SafeRouter m a) = m a
    subcompCombine (SafeRouterPath p1) (SafeRouterPath p2) =
        SafeRouterPath $
        p1 </> p2
    emptyRegistry = SafeRouterReg (emptyPathMap, [])
    rootPath = SafeRouterPath Empty
    defRoute (SafeRouterPath path) action (SafeRouterReg (m, cAll)) =
        SafeRouterReg
        ( insertPathMap (RouteHandle path (flipHVectElim action)) m
        , cAll
        )
    fallbackRoute routeDef (SafeRouterReg (m, cAll)) =
        SafeRouterReg (m, cAll ++ [routeDef])
    matchRoute (SafeRouterReg (m, cAll)) pathPieces =
        let matches = match m pathPieces
            matches' =
                if null matches
                then matches ++ (map (\f -> f pathPieces) cAll)
                else matches
        in zip (replicate (length matches') HM.empty) matches'


data Path (as :: [*]) where
  Empty :: Path '[] -- the empty path
  StaticCons :: T.Text -> Path as -> Path as -- append a static path piece to path
  VarCons :: (PathPiece a, Typeable a) => Path as -> Path (a ': as) -- append a param to path

pathToRep :: Path as -> Rep as
pathToRep Empty = RNil
pathToRep (StaticCons _ p) = pathToRep p
pathToRep (VarCons p) = RCons (pathToRep p)

data PathMap x =
  PathMap
  { pm_here :: [x]
  , pm_staticMap :: HM.HashMap T.Text (PathMap x)
  , pm_polyMap :: PM.PolyMap PathPiece PathMap x
  }

instance Functor PathMap where
  fmap f (PathMap h s p) = PathMap (fmap f h) (fmap (fmap f) s) (fmap f p)

instance Applicative PathMap where
  pure x = PathMap [x] mempty PM.empty
  PathMap h s p <*> y =
    let start = PathMap mempty (fmap (<*> y) s) (PM.updateAll (\a -> fmap flip a <*> y) p)
    in foldl' (\pm f -> pm `mappend` fmap f y) start h

instance Alternative PathMap where
  empty = emptyPathMap
  (<|>) = mappend

instance NFData x => NFData (PathMap x) where
  rnf (PathMap h s p) = rnf h `seq` rnf s `seq` PM.rnfHelper rnf p

emptyPathMap :: PathMap x
emptyPathMap = PathMap mempty mempty PM.empty

instance Monoid (PathMap x) where
  mempty = emptyPathMap
  mappend (PathMap h1 s1 p1) (PathMap h2 s2 p2) =
    PathMap (h1 `mappend` h2) (HM.unionWith mappend s1 s2) (PM.unionWith mappend p1 p2)

insertPathMap' :: Path ts -> (HVect ts -> x) -> PathMap x -> PathMap x
insertPathMap' path action (PathMap h s p) =
  case path of
    Empty -> PathMap (action HNil : h) s p
    StaticCons pathPiece path' ->
      let subPathMap = fromMaybe emptyPathMap (HM.lookup pathPiece s)
      in PathMap h (HM.insert pathPiece (insertPathMap' path' action subPathMap) s) p
    VarCons path' ->
      let alterFn = Just . insertPathMap' path' (\vs v -> action (v :&: vs))
                         . fromMaybe emptyPathMap
      in PathMap h s (PM.alter alterFn p)

singleton :: Path ts -> HVectElim ts x -> PathMap x
singleton path action = insertPathMap' path (HV.uncurry action) mempty

insertPathMap :: RouteHandle m a -> PathMap (m a) -> PathMap (m a)
insertPathMap (RouteHandle path action) = insertPathMap' path (HV.uncurry action)

match :: PathMap x -> [T.Text] -> [x]
match (PathMap h _ _) [] = h
match (PathMap _ s p) (pp:pps) =
  let staticMatches = maybeToList (HM.lookup pp s) >>= flip match pps
      varMatches = PM.lookupConcat (fromPathPiece pp)
                     (\piece pathMap' -> fmap ($ piece) (match pathMap' pps)) p
  in staticMatches ++ varMatches

-- | A route parameter
var :: (Typeable a, PathPiece a) => Path (a ': '[])
var = VarCons Empty

type Var a = Path (a ': '[])

-- | A static route piece
static :: String -> Path '[]
static s =
  let pieces = filter (not . T.null) $ T.splitOn "/" $ T.pack s
  in foldr StaticCons Empty pieces

instance (a ~ '[]) => IsString (Path a) where
    fromString = static

-- | The root of a path piece. Use to define a handler for "/"
root :: Path '[]
root = Empty

(</>) :: Path as -> Path bs -> Path (Append as bs)
(</>) Empty xs = xs
(</>) (StaticCons pathPiece xs) ys = (StaticCons pathPiece (xs </> ys))
(</>) (VarCons xs) ys = (VarCons (xs </> ys))

renderRoute :: Path as -> HVect as -> T.Text
renderRoute p h =
    T.intercalate "/" $ renderRoute' p h

renderRoute' :: Path as -> HVect as -> [T.Text]
renderRoute' Empty _ = []
renderRoute' (StaticCons pathPiece pathXs) paramXs =
    ( pathPiece : renderRoute' pathXs paramXs )
renderRoute' (VarCons pathXs) (val :&: paramXs) =
    ( toPathPiece val : renderRoute' pathXs paramXs)
renderRoute' _ _ =
    error "This will never happen."

parse :: Path as -> [T.Text] -> Maybe (HVect as)
parse Empty [] = Just HNil
parse _ [] = Nothing
parse path (pathComp : xs) =
    case path of
      Empty -> Nothing
      StaticCons pathPiece pathXs ->
          if pathPiece == pathComp
          then parse pathXs xs
          else Nothing
      VarCons pathXs ->
          case fromPathPiece pathComp of
            Nothing -> Nothing
            Just val ->
                let finish = parse pathXs xs
                in fmap (\parsedXs -> val :&: parsedXs) finish
