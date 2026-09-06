{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Web.Routing.SafeRouting where

#if MIN_VERSION_base(4,11,0)
#elif MIN_VERSION_base(4,9,0)
import Data.Semigroup
#elif MIN_VERSION_base(4,8,0)
import Data.Monoid ((<>))
#else
import Control.Applicative ((<$>))
import Data.Monoid (Monoid (..), (<>))
#endif
import Control.DeepSeq (NFData (..))
import Data.HVect hiding (length, null, reverse)
import qualified Data.HVect as HV
#if defined(javascript_HOST_ARCH)
-- hashable's Text instance calls CApiFFI symbols absent from the JS runtime.
-- Ordered lookup is portable and preserves the registry's matching order.
import qualified Data.Map.Strict as HM
#else
import qualified Data.HashMap.Strict as HM
#endif
import Data.List (findIndices, sortBy)
import Data.Maybe
import qualified Data.PolyMap as PM
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Web.HttpApiData

-- | How empty path segments are treated. The compatibility default ignores
-- every empty segment. Strict policies preserve internal and trailing slashes.
-- Redirects are performed by the HTTP adapter, using strict registry matching.
data SlashPolicy = IgnoreSlashes | StrictSlashes | RedirectTrailingSlashes
  deriving (Eq, Show, Read)

normalizeInternalPath :: SlashPolicy -> PathInternal as -> PathInternal as
normalizeInternalPath IgnoreSlashes (PI_StaticCons "" rest) = normalizeInternalPath IgnoreSlashes rest
normalizeInternalPath policy (PI_StaticCons piece rest) = PI_StaticCons piece (normalizeInternalPath policy rest)
normalizeInternalPath policy (PI_VarCons rest) = PI_VarCons (normalizeInternalPath policy rest)
normalizeInternalPath policy (PI_Wildcard rest) = PI_Wildcard (normalizeInternalPath policy rest)
normalizeInternalPath policy (PI_Extension left right) = PI_Extension (normalizeInternalPath policy left) (normalizeInternalPath policy right)
normalizeInternalPath policy (PI_Append left right) = PI_Append (normalizeInternalPath policy left) (normalizeInternalPath policy right)
normalizeInternalPath _ PI_Empty = PI_Empty

data RouteHandle m a
  = forall as. RouteHandle (PathInternal as) (HVectElim as (m a))

newtype HVectElim' x ts = HVectElim' {flipHVectElim :: HVectElim ts x}

type Registry m a = (PathMap (m a), [[T.Text] -> m a])

emptyRegistry :: Registry m a
emptyRegistry = (emptyPathMap, [])

defRoute :: PathInternal xs -> HVectElim' (m a) xs -> Registry m a -> Registry m a
defRoute path action (m, call) =
  ( insertPathMap (RouteHandle path (flipHVectElim action)) m,
    call
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

data PathInternal (as :: [*]) where
  PI_Empty :: PathInternal '[] -- the empty path
  PI_StaticCons :: T.Text -> PathInternal as -> PathInternal as -- append a static path piece to path
  PI_VarCons :: (FromHttpApiData a, Typeable a) => PathInternal as -> PathInternal (a ': as) -- append a param to path
  PI_Wildcard :: PathInternal as -> PathInternal (T.Text ': as) -- append the rest of the route
  PI_Extension :: PathInternal as -> PathInternal bs -> PathInternal (Append as bs)
  PI_Append :: PathInternal as -> PathInternal bs -> PathInternal (Append as bs)

data PathMap x = PathMap
  { pm_subComponents :: [[T.Text] -> x],
    pm_here :: [x],
#if defined(javascript_HOST_ARCH)
    pm_staticMap :: HM.Map T.Text (PathMap x),
#else
    pm_staticMap :: HM.HashMap T.Text (PathMap x),
#endif
    pm_polyMap :: PM.PolyMap FromHttpApiData PathMap x,
    pm_wildcards :: [T.Text -> x],
    pm_patterns :: [(Int, [T.Text] -> [x])]
  }

instance Functor PathMap where
  fmap f (PathMap c h s p w e) =
    PathMap (fmap f <$> c) (f <$> h) (fmap f <$> s) (f <$> p) (fmap f <$> w)
      [(priority, fmap f . matcher) | (priority, matcher) <- e]

instance NFData x => NFData (PathMap x) where
  rnf (PathMap c h s p w e) =
    rnf c `seq` rnf h `seq` rnf s `seq` PM.rnfHelper rnf p `seq` rnf w `seq` rnf e

emptyPathMap :: PathMap x
emptyPathMap = PathMap mempty mempty mempty PM.empty mempty mempty

instance Semigroup (PathMap x) where
  (PathMap c1 h1 s1 p1 w1 e1) <> (PathMap c2 h2 s2 p2 w2 e2) =
    PathMap (c1 <> c2) (h1 <> h2) (HM.unionWith (<>) s1 s2) (PM.unionWith (<>) p1 p2) (w1 <> w2)
      (orderPatterns $ e1 <> e2)

orderPatterns :: [(Int, a)] -> [(Int, a)]
orderPatterns = sortBy (\(a, _) (b, _) -> compare b a)

instance Monoid (PathMap x) where
  mempty = emptyPathMap
  mappend = (<>)

updatePathMap ::
  (forall y. (ctx -> y) -> PathMap y -> PathMap y) ->
  PathInternal ts ->
  (HVect ts -> ctx -> x) ->
  PathMap x ->
  PathMap x
updatePathMap updateFn path action pm@(PathMap c h s p w e) =
  case path of
    PI_Empty -> updateFn (action HNil) pm
    PI_StaticCons pathPiece path' ->
      let subPathMap = fromMaybe emptyPathMap (HM.lookup pathPiece s)
       in PathMap c h (HM.insert pathPiece (updatePathMap updateFn path' action subPathMap) s) p w e
    PI_VarCons path' ->
      let alterFn =
            Just . updatePathMap updateFn path' (\vs ctx v -> action (v :&: vs) ctx)
              . fromMaybe emptyPathMap
       in PathMap c h s (PM.alter alterFn p) w e
    PI_Wildcard PI_Empty ->
      let (PathMap _ (action' : _) _ _ _ _) = updateFn (\ctx rest -> action (rest :&: HNil) ctx) emptyPathMap
       in PathMap c h s p (action' : w) e
    PI_Wildcard _ -> error "Shouldn't happen"
    PI_Extension _ _ -> patternMap
    PI_Append _ _ -> patternMap
  where
    patternMap = pm { pm_patterns = orderPatterns $ (pathSpecificity path, matcher) : e }
    matcher pieces = case parsePrefix path pieces of
      Nothing -> []
      Just (args, remaining) -> match (updateFn (action args) emptyPathMap) remaining

insertPathMap' :: PathInternal ts -> (HVect ts -> x) -> PathMap x -> PathMap x
insertPathMap' path action =
  let updateHeres y (PathMap c h s p w e) = PathMap c (y () : h) s p w e
   in updatePathMap updateHeres path (const <$> action)

singleton :: PathInternal ts -> HVectElim ts x -> PathMap x
singleton path action = insertPathMap' path (HV.uncurry action) mempty

insertPathMap :: RouteHandle m a -> PathMap (m a) -> PathMap (m a)
insertPathMap (RouteHandle path action) = insertPathMap' path (HV.uncurry action)

insertSubComponent' :: PathInternal ts -> (HVect ts -> [T.Text] -> x) -> PathMap x -> PathMap x
insertSubComponent' path subComponent =
  let updateSubComponents y (PathMap c h s p w e) = PathMap (y : c) h s p w e
   in updatePathMap updateSubComponents path subComponent

insertSubComponent :: Functor m => RouteHandle m ([T.Text] -> a) -> PathMap (m a) -> PathMap (m a)
insertSubComponent (RouteHandle path comp) =
  insertSubComponent' path (fmap (\m ps -> fmap ($ ps) m) (HV.uncurry comp))

match :: PathMap x -> [T.Text] -> [x]
match (PathMap c h s p w e) pieces =
  map ($ pieces) c
    ++ case pieces of
      [] -> h ++ concatMap (($ pieces) . snd) e ++ fmap ($ "") w
      (pp : pps) ->
        let staticMatches = maybeToList (HM.lookup pp s) >>= flip match pps
            varMatches =
              PM.lookupConcat
                (either (const Nothing) Just $ parseUrlPiece pp)
                (\piece pathMap' -> fmap ($ piece) (match pathMap' pps))
                p
            routeRest = combineRoutePieces pieces
            wildcardMatches = fmap ($ routeRest) w
            extensionMatches = concatMap (($ pieces) . snd) e
         in staticMatches ++ extensionMatches ++ varMatches ++ wildcardMatches

(</!>) :: PathInternal as -> PathInternal bs -> PathInternal (Append as bs)
(</!>) PI_Empty xs = xs
(</!>) (PI_StaticCons pathPiece xs) ys = PI_StaticCons pathPiece (xs </!> ys)
(</!>) (PI_VarCons xs) ys = PI_VarCons (xs </!> ys)
(</!>) (PI_Wildcard _) _ = error "Shouldn't happen"
(</!>) path@(PI_Extension _ _) ys = PI_Append path ys
(</!>) path@(PI_Append _ _) ys = PI_Append path ys

combineRoutePieces :: [T.Text] -> T.Text
combineRoutePieces = T.intercalate "/"

parse :: PathInternal as -> [T.Text] -> Maybe (HVect as)
parse path pieces = do
  (args, remaining) <- parsePrefix path pieces
  if null remaining then Just args else Nothing

parsePrefix :: PathInternal as -> [T.Text] -> Maybe (HVect as, [T.Text])
parsePrefix PI_Empty pieces = Just (HNil, pieces)
parsePrefix (PI_Wildcard PI_Empty) pieces = Just (combineRoutePieces pieces :&: HNil, [])
parsePrefix (PI_Wildcard _) _ = error "Shouldn't happen"
parsePrefix (PI_Append left right) pieces = do
  (leftArgs, rest) <- parsePrefix left pieces
  (rightArgs, remaining) <- parsePrefix right rest
  pure (leftArgs <++> rightArgs, remaining)
parsePrefix (PI_Extension left right) pieces =
  case splitAt (max 0 $ pathPieceCount left - 1) pieces of
    (prefix, joined : rest) -> listToMaybe
      [ (leftArgs <++> rightArgs, remaining)
      | (base, extension) <- extensionSplits right joined,
        Just leftArgs <- [parse left (if pathPieceCount left == 0 then [] else prefix ++ [base])],
        pathPieceCount left /= 0 || T.null base,
        pathPieceCount right /= 0 || T.null extension,
        Just (rightArgs, remaining) <- [parsePrefix right (if pathPieceCount right == 0 then rest else extension : rest)]
      ]
    _ -> Nothing
parsePrefix _ [] = Nothing
parsePrefix (PI_StaticCons expected rest) (piece : pieces)
  | expected == piece = parsePrefix rest pieces
  | otherwise = Nothing
parsePrefix (PI_VarCons rest) (piece : pieces) = do
  value <- either (const Nothing) Just $ parseUrlPiece piece
  (args, remaining) <- parsePrefix rest pieces
  pure (value :&: args, remaining)

-- Fixed suffixes have one possible split; avoid trying every dot in a long
-- filename when the literal extension is absent.
extensionSplits :: PathInternal as -> T.Text -> [(T.Text, T.Text)]
extensionSplits (PI_StaticCons extension _) piece =
  [(base, extension) | Just base <- [T.stripSuffix ("." <> extension) piece]]
extensionSplits PI_Empty piece = [(base, "") | Just base <- [T.stripSuffix "." piece]]
extensionSplits _ piece = dotSplits piece

-- Rightmost valid split keeps dots in a basename, while allowing a fixed
-- multi-dot suffix such as tar.gz or a custom typed extension parser.
dotSplits :: T.Text -> [(T.Text, T.Text)]
dotSplits piece = [(T.take i piece, T.drop (i + 1) piece) | i <- reverse $ findIndices (== '.') $ T.unpack piece]

pathPieceCount :: PathInternal as -> Int
pathPieceCount PI_Empty = 0
pathPieceCount (PI_StaticCons _ rest) = 1 + pathPieceCount rest
pathPieceCount (PI_VarCons rest) = 1 + pathPieceCount rest
pathPieceCount (PI_Wildcard _) = 1
pathPieceCount (PI_Append left right) = pathPieceCount left + pathPieceCount right
pathPieceCount (PI_Extension left right) = max 1 (pathPieceCount left) + max 1 (pathPieceCount right) - 1

pathSpecificity :: PathInternal as -> Int
pathSpecificity PI_Empty = 0
pathSpecificity (PI_StaticCons piece rest) = T.length piece + pathSpecificity rest
pathSpecificity (PI_VarCons rest) = pathSpecificity rest
pathSpecificity (PI_Wildcard rest) = pathSpecificity rest
pathSpecificity (PI_Append left right) = pathSpecificity left + pathSpecificity right
pathSpecificity (PI_Extension left right) = 1 + pathSpecificity left + pathSpecificity right
