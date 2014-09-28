{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Web.Spock.Internal.SafeRouting where

import Web.Spock.Internal.AbstractRouter

import Data.HList
import Data.Maybe
import Data.Monoid
import Data.String
import Web.PathPieces
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

data RouteHandle m a
   = forall as. RouteHandle (Path as) (HListElim as (m a))

instance IsPath Path where
    type CaptureFreePath Path = '[]
    combineSubcomp = (</>)

newtype HListElim' x ts = HListElim' { flipHListElim :: HListElim ts x }

typesafeRegistry :: AnyRouteRegistryIf Path (HListElim' (m a)) m a (PathMap (m a))
typesafeRegistry =
    AnyRouteRegistryIf
    { rr_emptyRegistry = emptyPathMap
    , rr_rootPath = Empty
    , rr_defRoute =
        \path action m ->
            insertPathMap (RouteHandle path (flipHListElim action)) m
    , rr_matchRoute =
        \m pathPieces ->
            let matches = match m pathPieces
            in zip (replicate (length matches) HM.empty) matches
    }

type family HListElim (ts :: [*]) (a :: *) :: *
type instance HListElim '[] a = a
type instance HListElim (t ': ts) a = t -> HListElim ts a

hListUncurry :: HListElim ts a -> HList ts -> a
hListUncurry f HNil = f
hListUncurry f (HCons x xs) = hListUncurry (f x) xs

data Path (as :: [*]) where
  Empty :: Path '[] -- the empty path
  StaticCons :: T.Text -> Path as -> Path as -- append a static path piece to path
  VarCons :: (PathPiece a, Typeable a) => Path as -> Path (a ': as) -- append a param to path

data PolyMap x where
  PMNil :: PolyMap x
  PMCons :: (Typeable a, PathPiece a) => PathMap (a -> x) -> PolyMap x -> PolyMap x

data Id a = Id { getId :: a }

castDefType :: (Typeable a, Typeable b) => (a -> c) -> Maybe (b -> c)
castDefType x = fmap getId (gcast1 (Id x))

insertPolyMap ::
  (Typeable a, PathPiece a)
  => Path ts
  -> (a -> HList ts -> x)
  -> PolyMap x
  -> PolyMap x
insertPolyMap path (action :: a -> HList ts -> x) polyMap =
  case polyMap of
    PMNil -> PMCons (insertPathMap' path (flip action) emptyPathMap) PMNil
    PMCons (pathMap :: PathMap (b -> x)) polyMap' ->
        case castDefType action of
          Just action' ->
              PMCons (insertPathMap' path (flip action') pathMap) polyMap'
          Nothing ->
               PMCons pathMap (insertPolyMap path action polyMap')

lookupPolyMap :: T.Text -> [T.Text] -> PolyMap x -> [x]
lookupPolyMap _ _ PMNil = []
lookupPolyMap pp pps (PMCons pathMap polyMap') =
  maybeToList (fromPathPiece pp) >>= \val -> fmap ($ val) (match pathMap pps)
  ++ lookupPolyMap pp pps polyMap'


emptyPolyMap :: PolyMap x
emptyPolyMap = PMNil

data PathMap x = PathMap [x] (HM.HashMap T.Text (PathMap x)) (PolyMap x)

emptyPathMap :: PathMap x
emptyPathMap = PathMap mempty mempty emptyPolyMap

insertPathMap' :: Path ts -> (HList ts -> x) -> PathMap x -> PathMap x
insertPathMap' path action (PathMap as m pm) =
  case path of
    Empty -> PathMap (action HNil : as) m pm
    StaticCons pathPiece xs ->
      let subPathMap = fromMaybe emptyPathMap (HM.lookup pathPiece m)
      in PathMap as (HM.insert pathPiece (insertPathMap' xs action subPathMap) m) pm
    VarCons xs -> PathMap as m (insertPolyMap xs (\v vs -> action (HCons v vs)) pm)

insertPathMap :: RouteHandle m a -> PathMap (m a) -> PathMap (m a)
insertPathMap (RouteHandle path action) = insertPathMap' path (hListUncurry action)

match :: PathMap x -> [T.Text] -> [x]
match (PathMap as _ _) [] = as
match (PathMap _ m pm) (pp:pps) =
  let staticMatches = maybeToList (HM.lookup pp m) >>= flip match pps
      varMatches = lookupPolyMap pp pps pm
  in staticMatches ++ varMatches

-- | A route parameter
var :: (Typeable a, PathPiece a) => Path (a ': '[])
var = VarCons Empty

type Var a = Path (a ': '[])

-- | A static route piece
static :: String -> Path '[]
static s = StaticCons (T.pack s) Empty

instance (a ~ '[]) => IsString (Path a) where
    fromString = static

-- | The root of a path piece. Use to define a handler for "/"
root :: Path '[]
root = Empty

(</>) :: Path as -> Path bs -> Path (HAppendList as bs)
(</>) Empty xs = xs
(</>) (StaticCons pathPiece xs) ys = (StaticCons pathPiece (xs </> ys))
(</>) (VarCons xs) ys = (VarCons (xs </> ys))

renderRoute :: Path as -> HList as -> T.Text
renderRoute p h =
    T.intercalate "/" $ renderRoute' p h

renderRoute' :: Path as -> HList as -> [T.Text]
renderRoute' Empty _ = []
renderRoute' (StaticCons pathPiece pathXs) paramXs =
    ( pathPiece : renderRoute' pathXs paramXs )
renderRoute' (VarCons pathXs) (HCons val paramXs) =
    ( toPathPiece val : renderRoute' pathXs paramXs)
renderRoute' _ _ =
    error "This will never happen."

parse :: Path as -> [T.Text] -> Maybe (HList as)
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
                in fmap (\parsedXs -> HCons val parsedXs) finish
