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
import qualified Data.Text.Encoding as T
import Data.Typeable (Typeable)
import Network.HTTP.Types.URI (urlEncode)
import Web.HttpApiData
import Web.Routing.SafeRouting

data PathState = Open | Closed

data Path (as :: [*]) (pathState :: PathState) where
  Empty :: Path '[] 'Open
  StaticCons :: T.Text -> Path as ps -> Path as ps
  VarCons :: (FromHttpApiData a, Typeable a) => Path as ps -> Path (a ': as) ps
  Wildcard :: Path as 'Open -> Path (T.Text ': as) 'Closed
  WithExtension :: Path as 'Open -> Path bs 'Open -> Path (Append as bs) 'Open
  AppendPath :: Path as 'Open -> Path bs ps -> Path (Append as bs) ps

toInternalPath :: Path as pathState -> PathInternal as
toInternalPath Empty = PI_Empty
toInternalPath (StaticCons t p) = PI_StaticCons t (toInternalPath p)
toInternalPath (VarCons p) = PI_VarCons (toInternalPath p)
toInternalPath (Wildcard p) = PI_Wildcard (toInternalPath p)
toInternalPath (WithExtension left right) = PI_Extension (toInternalPath left) (toInternalPath right)
toInternalPath (AppendPath left right) = PI_Append (toInternalPath left) (toInternalPath right)

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
    appendSlash (WithExtension left Empty) = WithExtension left (StaticCons "" $ StaticCons "" Empty)
    appendSlash (WithExtension left right) = WithExtension left (appendSlash right)
    appendSlash (AppendPath left right) = AppendPath left (appendSlash right)

-- | Matches the rest of the route. Should be the last part of the path.
wildcard :: Path '[T.Text] 'Closed
wildcard = Wildcard Empty

(</>) :: Path as 'Open -> Path bs ps2 -> Path (Append as bs) ps2
(</>) Empty xs = xs
(</>) (StaticCons pathPiece xs) ys = StaticCons pathPiece (xs </> ys)
(</>) (VarCons xs) ys = VarCons (xs </> ys)
(</>) path@(WithExtension _ _) ys = AppendPath path ys
(</>) path@(AppendPath _ _) ys = AppendPath path ys

-- | Join the last segment on the left and the first on the right with a dot.
-- Both sides may contain typed captures: @var <.> "txt"@ or @var <.> var@.
-- Matching tries the rightmost dot first and accepts the first split whose
-- typed parsers and literals succeed. Use a custom capture type to restrict
-- extensions; a Text capture accepts any text, including an empty extension.
-- Static routes take precedence over extensions, then plain captures and
-- wildcards. More literal characters give an extension pattern priority.
(<.>) :: Path as 'Open -> Path bs 'Open -> Path (Append as bs) 'Open
(<.>) (StaticCons base Empty) (StaticCons extension rest) = StaticCons (base <> "." <> extension) rest
(<.>) path@(StaticCons piece rest) right = case rest of
  Empty -> WithExtension path right
  _ -> StaticCons piece (rest <.> right)
(<.>) path@(VarCons rest) right = case rest of
  Empty -> WithExtension path right
  _ -> VarCons (rest <.> right)
(<.>) left right = WithExtension left right
infixl 8 <.>

pathToRep :: Path as ps -> Rep as
pathToRep Empty = RNil
pathToRep (StaticCons _ p) = pathToRep p
pathToRep (VarCons p) = RCons (pathToRep p)
pathToRep (Wildcard p) = RCons (pathToRep p)
pathToRep (WithExtension left right) = appendRep (pathToRep left) (pathToRep right)
pathToRep (AppendPath left right) = appendRep (pathToRep left) (pathToRep right)

appendRep :: Rep as -> Rep bs -> Rep (Append as bs)
appendRep RNil right = right
appendRep (RCons left) right = RCons (appendRep left right)

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
normalizePath policy (WithExtension left right) = WithExtension (normalizePath policy left) (normalizePath policy right)
normalizePath policy (AppendPath left right) = AppendPath (normalizePath policy left) (normalizePath policy right)
normalizePath _ Empty = Empty

renderRoute' :: AllHave ToHttpApiData as => Path as 'Open -> HVect as -> [T.Text]
renderRoute' path = fst . renderPieces path . captureTexts

captureTexts :: AllHave ToHttpApiData as => HVect as -> [T.Text]
captureTexts HNil = []
captureTexts (value :&: rest) = toUrlPiece value : captureTexts rest

renderPieces :: Path as 'Open -> [T.Text] -> ([T.Text], [T.Text])
renderPieces Empty values = ([], values)
renderPieces (StaticCons piece rest) values = let (pieces, remaining) = renderPieces rest values in (piece : pieces, remaining)
renderPieces (VarCons rest) (value : values) = let (pieces, remaining) = renderPieces rest values in (value : pieces, remaining)
renderPieces (VarCons _) [] = error "renderPieces: internal capture arity mismatch"
renderPieces (WithExtension left right) values =
  let (leftPieces, rest) = renderPieces left values
      (rightPieces, remaining) = renderPieces right rest
  in (joinWithDot leftPieces rightPieces, remaining)
renderPieces (AppendPath left right) values =
  let (leftPieces, rest) = renderPieces left values
      (rightPieces, remaining) = renderPieces right rest
  in (leftPieces ++ rightPieces, remaining)

joinWithDot :: [T.Text] -> [T.Text] -> [T.Text]
joinWithDot [] [] = ["."]
joinWithDot [] (first : rest) = ("." <> first) : rest
joinWithDot [lastPiece] [] = [lastPiece <> "."]
joinWithDot [lastPiece] (first : rest) = (lastPiece <> "." <> first) : rest
joinWithDot (piece : rest) right = piece : joinWithDot rest right

-- | Percent-encode each complete segment after joining extension pieces.
-- This protects slashes, spaces, percent signs, query delimiters and Unicode
-- in captures. The unencoded 'renderRoute' remains available for compatibility.
renderRouteEncoded :: AllHave ToHttpApiData as => Path as 'Open -> HVect as -> T.Text
renderRouteEncoded = renderRouteEncodedWith IgnoreSlashes

renderRouteEncodedWith :: AllHave ToHttpApiData as => SlashPolicy -> Path as 'Open -> HVect as -> T.Text
renderRouteEncodedWith policy path = combineRoutePieces . map (T.decodeUtf8 . urlEncode True . T.encodeUtf8)
  . renderRoute' (normalizePath policy path)
