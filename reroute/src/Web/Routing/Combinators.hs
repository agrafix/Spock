{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Web.Routing.Combinators where

import Data.HVect (Append)
import Data.String
import Data.Typeable (Typeable)
import Web.PathPieces
import qualified Data.Text as T

import Web.Routing.SafeRouting

type Var a = Path (a ': '[])

-- | A route parameter
var :: (Typeable a, PathPiece a) => Path (a ': '[])
var = VarCons Empty

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

-- | Matches the rest of the route. Should be the last part of the path.
theRest :: Path '[T.Text]
theRest = Wildcard Empty

(</>) :: Path as -> Path bs -> Path (Append as bs)
(</>) Empty xs = xs
(</>) (StaticCons pathPiece xs) ys = (StaticCons pathPiece (xs </> ys))
(</>) (VarCons xs) ys = (VarCons (xs </> ys))
(</>) (Wildcard _) _ = error "Wildcard should be the last part of the path"


