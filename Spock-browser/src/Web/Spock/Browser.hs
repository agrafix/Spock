{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Typed routes shared by native tests and GHC's JavaScript backend. Use
-- "Web.Spock.Browser.History" to attach a compiled router to browser navigation.
module Web.Spock.Browser
  ( Routes, Router, route, compileRoutes, dispatch,
    Location (..), NavigationError (..), parseLocation, renderPath,
    Path, PathState (..), root, static, var, wildcard, trailingSlash,
    (<//>), (<.>), SlashPolicy (..)
  ) where

import Control.Monad (unless)
import Data.HVect (AllHave, Append, HVect, HVectElim)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types.URI (urlDecode)
import Web.HttpApiData (ToHttpApiData)
import Web.Routing.Combinators hiding ((</>))
import qualified Web.Routing.Combinators as R
import Web.Routing.Router
import Web.Routing.SafeRouting (HVectElim' (..), SlashPolicy (..))

-- | A registry of navigation handlers, using the same typed captures as Spock.
type Routes = RegistryT IO () () () IO ()

-- | A compiled registry. 'dispatch' executes its first matching handler.
newtype Router = Router ([T.Text] -> [IO ()])

-- | Register a route. Handler arguments follow the typed path captures.
route :: Path as ps -> HVectElim as (IO ()) -> Routes
route path handler = hookRouteAnyMethod (toInternalPath path) (HVectElim' handler)

-- | Compile definitions once. IgnoreSlashes is Spock's compatibility policy.
-- StrictSlashes preserves empty segments. RedirectTrailingSlashes uses strict
-- matching here; canonical HTTP redirects belong to the server adapter.
compileRoutes :: SlashPolicy -> Routes -> IO Router
compileRoutes policy definitions = do
  (_, match, _) <- runRegistryWith policy definitions
  pure $ Router (match ())

-- | Run the first match, returning False when no route matched. Handler
-- exceptions propagate; no alternative handler runs after a successful match.
dispatch :: Router -> Location -> IO Bool
dispatch (Router match) location = case match (locationSegments location) of
  [] -> pure False
  handler : _ -> handler >> pure True

-- | A validated local URL. Query and fragment remain encoded, with their
-- leading '?' and '#'. Path captures are percent-decoded once, segment by
-- segment, so an encoded slash remains inside its captured value.
data Location = Location
  { locationPathname :: T.Text,
    locationSegments :: [T.Text],
    locationQuery :: T.Text,
    locationFragment :: T.Text
  } deriving (Eq, Show)

-- | Failures omit the rejected URL and any sensitive query values.
data NavigationError = InvalidLocation | OutsideScope | HistoryUnavailable
  | AlreadyMounted | RouterStopped
  deriving (Eq, Show)

-- | Parse an absolute local path with optional query/fragment. Reject external
-- origins, malformed escapes/UTF-8, and dot segments browsers would normalize.
parseLocation :: T.Text -> Either NavigationError Location
parseLocation value = do
  unless (T.isPrefixOf "/" value && not (T.isPrefixOf "//" value)
    && not (T.any (\c -> c <= ' ' || c == '\\' || c == '\DEL') value)) $ Left InvalidLocation
  let (beforeFragment, fragment) = T.breakOn "#" value
      (pathname, query) = T.breakOn "?" beforeFragment
      pieces = if pathname == "/" then [] else T.splitOn "/" (T.drop 1 pathname)
  decoded <- traverse decodePiece pieces
  unless (all (`notElem` [".", ".."]) decoded) $ Left InvalidLocation
  pure $ Location pathname decoded query fragment
  where
    decodePiece piece = do
      unless (validEscapes $ T.unpack piece) $ Left InvalidLocation
      either (const $ Left InvalidLocation) Right $ T.decodeUtf8' $ urlDecode False $ T.encodeUtf8 piece
    validEscapes [] = True
    validEscapes ('%' : a : b : rest) = a `elem` hex && b `elem` hex && validEscapes rest
    validEscapes ('%' : _) = False
    validEscapes (_ : rest) = validEscapes rest
    hex = "0123456789abcdefABCDEF" :: String

-- | Render a safe local href using the same path and slash policy as the
-- registry. Values are encoded as complete segments, including extensions.
renderPath :: AllHave ToHttpApiData as => SlashPolicy -> Path as 'Open -> HVect as -> Either NavigationError T.Text
renderPath policy path arguments = do
  let href = "/" <> renderRouteEncodedWith policy path arguments
  _ <- parseLocation href
  pure href

-- | Append path components, using Spock's familiar operator spelling.
(<//>) :: Path as 'Open -> Path bs ps -> Path (Append as bs) ps
(<//>) = (R.</>)
infixl 5 <//>
