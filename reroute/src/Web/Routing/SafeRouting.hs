{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Web.Routing.SafeRouting where

import qualified Data.PolyMap as PM
import Data.HVect hiding (null, length)
import qualified Data.HVect as HV

import Data.Maybe
#if MIN_VERSION_base(4,8,0)
import Data.Monoid ((<>))
#else
import Data.Monoid (Monoid (..), (<>))
import Control.Applicative ((<$>))
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

type Registry m a = (PathMap (m a), [[T.Text] -> m a])

emptyRegistry :: Registry m a
emptyRegistry = (emptyPathMap, [])

subcompCombine :: Path '[] -> Path xs -> Path xs
subcompCombine = (</>)

rootPath :: Path '[]
rootPath = Empty

defRoute :: Path xs -> HVectElim' (m a) xs -> Registry m a -> Registry m a
defRoute path action (m, call) =
    ( insertPathMap (RouteHandle path (flipHVectElim action)) m
    , call
    )

fallbackRoute :: ([T.Text] -> m a) -> Registry m a -> Registry m a
fallbackRoute routeDef (m, call) = (m, call ++ [routeDef])

matchRoute :: Registry m a -> [T.Text] -> [m a]
matchRoute (m, cAll) pathPieces =
    let matches = match m pathPieces
        matches' =
            if null matches
            then matches ++ (map (\f -> f pathPieces) cAll)
            else matches
    in matches'

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
  { pm_subComponents :: [[T.Text] -> x]
  , pm_here :: [x]
  , pm_staticMap :: HM.HashMap T.Text (PathMap x)
  , pm_polyMap :: PM.PolyMap PathPiece PathMap x
  }

instance Functor PathMap where
  fmap f (PathMap c h s p) =
      PathMap (fmap f <$> c) (f <$> h) (fmap f <$> s) (f <$> p)

instance NFData x => NFData (PathMap x) where
  rnf (PathMap c h s p) =
      rnf c `seq` rnf h `seq` rnf s `seq` PM.rnfHelper rnf p

emptyPathMap :: PathMap x
emptyPathMap = PathMap mempty mempty mempty PM.empty

instance Monoid (PathMap x) where
  mempty = emptyPathMap
  mappend (PathMap c1 h1 s1 p1) (PathMap c2 h2 s2 p2) =
    PathMap (c1 <> c2) (h1 <> h2) (HM.unionWith (<>) s1 s2) (PM.unionWith (<>) p1 p2)

updatePathMap
  :: (forall y. (ctx -> y) -> PathMap y -> PathMap y)
  -> Path ts
  -> (HVect ts -> ctx -> x)
  -> PathMap x
  -> PathMap x
updatePathMap updateFn path action pm@(PathMap c h s p) =
  case path of
    Empty -> updateFn (action HNil) pm
    StaticCons pathPiece path' ->
      let subPathMap = fromMaybe emptyPathMap (HM.lookup pathPiece s)
      in PathMap c h (HM.insert pathPiece (updatePathMap updateFn path' action subPathMap) s) p
    VarCons path' ->
      let alterFn = Just . updatePathMap updateFn path' (\vs ctx v -> action (v :&: vs) ctx)
                         . fromMaybe emptyPathMap
      in PathMap c h s (PM.alter alterFn p)

insertPathMap' :: Path ts -> (HVect ts -> x) -> PathMap x -> PathMap x
insertPathMap' path action =
  let updateHeres y (PathMap c h s p) = PathMap c (y () : h) s p
  in updatePathMap updateHeres path (const <$> action)

singleton :: Path ts -> HVectElim ts x -> PathMap x
singleton path action = insertPathMap' path (HV.uncurry action) mempty

insertPathMap :: RouteHandle m a -> PathMap (m a) -> PathMap (m a)
insertPathMap (RouteHandle path action) = insertPathMap' path (HV.uncurry action)

insertSubComponent' :: Path ts -> (HVect ts -> [T.Text] -> x) -> PathMap x -> PathMap x
insertSubComponent' path subComponent =
  let updateSubComponents y (PathMap c h s p) = PathMap (y : c) h s p
  in updatePathMap updateSubComponents path subComponent

insertSubComponent :: Functor m => RouteHandle m ([T.Text] -> a) -> PathMap (m a) -> PathMap (m a)
insertSubComponent (RouteHandle path comp) =
  insertSubComponent' path (fmap (\m ps -> fmap ($ ps) m) (HV.uncurry comp))

match :: PathMap x -> [T.Text] -> [x]
match (PathMap c h s p) pieces =
  map ($ pieces) c ++
  case pieces of
    [] -> h
    (pp:pps) ->
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
#if __GLASGOW_HASKELL__ < 800
renderRoute' _ _ =
    error "This will never happen."
#endif

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
