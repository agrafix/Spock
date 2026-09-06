{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Web.Routing.Combinators where

import Data.HVect
import Data.Maybe (fromMaybe)
import Data.String
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Web.HttpApiData
import Web.Routing.SafeRouting

data PathState = Open | Closed

data Path (as :: [*]) (pathState :: PathState) where
  Empty :: Path '[] 'Open
  StaticCons :: T.Text -> Path as ps -> Path as ps
  VarCons :: (FromHttpApiData a, Typeable a) => Path as ps -> Path (a ': as) ps
  Wildcard :: Path as 'Open -> Path (T.Text ': as) 'Closed

toInternalPath :: Path as pathState -> PathInternal as
toInternalPath Empty = PI_Empty
toInternalPath (StaticCons t p) = PI_StaticCons t (toInternalPath p)
toInternalPath (VarCons p) = PI_VarCons (toInternalPath p)
toInternalPath (Wildcard p) = PI_Wildcard (toInternalPath p)

type Var a = Path (a ': '[]) 'Open

-- | A variant of 'Either' with a 'FromHttpApiData' definition that tries both branches without a prefix.
-- Useful to define routes with 'var's that should work with different types.
data AltVar a b = AvLeft a | AvRight b
  deriving (Show, Eq, Read, Ord)

instance (FromHttpApiData a, FromHttpApiData b) => FromHttpApiData (AltVar a b) where
  parseUrlPiece val =
    case parseUrlPiece val of
      Left err ->
        case parseUrlPiece val of
          Left err2 -> Left (err <> " " <> err2)
          Right ok -> Right (AvRight ok)
      Right ok -> Right (AvLeft ok)

-- | A route parameter
var :: (Typeable a, FromHttpApiData a) => Path (a ': '[]) 'Open
var = VarCons Empty

-- | A static route piece. One leading slash is optional. Empty internal and
-- trailing pieces are retained for strict routing; compatibility routing and
-- 'renderRoute' ignore them. Use 'renderRouteWith' with the application's policy.
static :: String -> Path '[] 'Open
static s =
  let relative = fromMaybe (T.pack s) $ T.stripPrefix "/" $ T.pack s
      pieces = if T.null relative then [] else T.splitOn "/" relative
   in foldr StaticCons Empty pieces

instance (a ~ '[], pathState ~ 'Open) => IsString (Path a pathState) where
  fromString = static

-- | The root of a path piece. Use to define a handler for "/"
root :: Path '[] 'Open
root = Empty

-- | Require a trailing slash in strict routing, including after a capture.
-- Root remains root, and a path already ending in a slash is unchanged.
trailingSlash :: Path as 'Open -> Path as 'Open
trailingSlash Empty = Empty
trailingSlash path = appendSlash path
  where
    appendSlash :: Path xs 'Open -> Path xs 'Open
    appendSlash Empty = StaticCons "" Empty
    appendSlash (StaticCons "" Empty) = StaticCons "" Empty
    appendSlash (StaticCons piece rest) = StaticCons piece (appendSlash rest)
    appendSlash (VarCons rest) = VarCons (appendSlash rest)

-- | Matches the rest of the route. Should be the last part of the path.
wildcard :: Path '[T.Text] 'Closed
wildcard = Wildcard Empty

(</>) :: Path as 'Open -> Path bs ps2 -> Path (Append as bs) ps2
(</>) Empty xs = xs
(</>) (StaticCons pathPiece xs) ys = StaticCons pathPiece (xs </> ys)
(</>) (VarCons xs) ys = VarCons (xs </> ys)

pathToRep :: Path as ps -> Rep as
pathToRep Empty = RNil
pathToRep (StaticCons _ p) = pathToRep p
pathToRep (VarCons p) = RCons (pathToRep p)
pathToRep (Wildcard p) = RCons (pathToRep p)

renderRoute :: AllHave ToHttpApiData as => Path as 'Open -> HVect as -> T.Text
renderRoute = renderRouteWith IgnoreSlashes

-- | Render with the same empty-segment policy used by the registry. Values are
-- URL pieces, as with 'renderRoute'; this function does not percent-encode them.
renderRouteWith :: AllHave ToHttpApiData as => SlashPolicy -> Path as 'Open -> HVect as -> T.Text
renderRouteWith policy p = combineRoutePieces . renderRoute' (normalizePath policy p)

normalizePath :: SlashPolicy -> Path as ps -> Path as ps
normalizePath IgnoreSlashes (StaticCons "" rest) = normalizePath IgnoreSlashes rest
normalizePath policy (StaticCons piece rest) = StaticCons piece (normalizePath policy rest)
normalizePath policy (VarCons rest) = VarCons (normalizePath policy rest)
normalizePath policy (Wildcard rest) = Wildcard (normalizePath policy rest)
normalizePath _ Empty = Empty

renderRoute' :: AllHave ToHttpApiData as => Path as 'Open -> HVect as -> [T.Text]
renderRoute' Empty _ = []
renderRoute' (StaticCons pathPiece pathXs) paramXs =
  (pathPiece : renderRoute' pathXs paramXs)
renderRoute' (VarCons pathXs) (val :&: paramXs) =
  (toUrlPiece val : renderRoute' pathXs paramXs)

#if __GLASGOW_HASKELL__ < 800
renderRoute' _ _ =
    error "This will never happen."
#endif
