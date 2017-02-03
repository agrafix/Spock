{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Web.Routing.Combinators where

import Data.HVect
import Data.String
import Data.Typeable (Typeable)
import Web.HttpApiData
import qualified Data.Text as T

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

-- | A route parameter
var :: (Typeable a, FromHttpApiData a) => Path (a ': '[]) 'Open
var = VarCons Empty

-- | A static route piece
static :: String -> Path '[] 'Open
static s =
  let pieces = filter (not . T.null) $ T.splitOn "/" $ T.pack s
  in foldr StaticCons Empty pieces

instance (a ~ '[], pathState ~ 'Open) => IsString (Path a pathState) where
    fromString = static

-- | The root of a path piece. Use to define a handler for "/"
root :: Path '[] 'Open
root = Empty

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
renderRoute p = combineRoutePieces . renderRoute' p

renderRoute' :: AllHave ToHttpApiData as => Path as 'Open -> HVect as -> [T.Text]
renderRoute' Empty _ = []
renderRoute' (StaticCons pathPiece pathXs) paramXs =
    ( pathPiece : renderRoute' pathXs paramXs )
renderRoute' (VarCons pathXs) (val :&: paramXs) =
    ( toUrlPiece val : renderRoute' pathXs paramXs)
#if __GLASGOW_HASKELL__ < 800
renderRoute' _ _ =
    error "This will never happen."
#endif
