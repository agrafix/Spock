{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Spock.Routing where

import Data.Hashable
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Text.Regex as Regex
import qualified Data.HashMap.Strict as HM
import Debug.Trace

type ParamMap = HM.HashMap CaptureVar T.Text

newtype CaptureVar
      = CaptureVar { unCaptureVar :: T.Text }
      deriving (Show, Eq, Hashable)

data RegexWrapper
   = RegexWrapper
   { rw_regex :: !Regex.Regex
   , rw_original :: !T.Text
   }

instance Eq RegexWrapper where
    r1 == r2 =
        rw_original r1 == rw_original r2

instance Show RegexWrapper where
    show (RegexWrapper _ x) = show x

data RouteNode
   = RouteNodeRegex !CaptureVar !RegexWrapper
   | RouteNodeCapture !CaptureVar
   | RouteNodeText !T.Text
   | RouteNodeRoot
   deriving (Show, Eq)

data RouteData a
   = RouteData
   { rd_node :: !RouteNode
   , rd_data :: Maybe a
   }
   deriving (Show, Eq)

data RoutingTree a
   = RoutingTree
   { rt_node :: !(RouteData a)
   , rt_children :: !(V.Vector (RoutingTree a))
   }
   deriving (Show, Eq)

buildRegex :: T.Text -> RegexWrapper
buildRegex t =
    RegexWrapper (Regex.mkRegex $ T.unpack t) t

emptyRoutingTree :: RoutingTree a
emptyRoutingTree =
    RoutingTree (RouteData RouteNodeRoot Nothing) V.empty

mergeData :: Maybe a -> Maybe a -> Maybe a
mergeData (Just _) (Just _) =
    error "Spock error: Don't define the same route twice!"
mergeData (Just a) _ = Just a
mergeData _ (Just b) = Just b
mergeData _ _ = Nothing

addToRoutingTree :: T.Text -> a -> RoutingTree a -> RoutingTree a
addToRoutingTree route dat currTree =
    let applyTree [] tree = tree
        applyTree (current:xs) tree =
            let children = V.map (\(RoutingTree d _) -> rd_node d) (rt_children tree)
                currentDat =
                    case xs of
                      [] -> Just dat
                      _ -> Nothing
                children' =
                    case V.findIndex (==current) children of
                      Nothing ->
                          let h = applyTree xs $ RoutingTree (RouteData current currentDat) V.empty
                          in V.cons h (rt_children tree)
                      Just idx ->
                          let origNode = (V.!) (rt_children tree) idx
                              matchingNode = rt_node $ origNode
                              appliedNode = matchingNode { rd_data = mergeData (rd_data matchingNode) currentDat }
                          in V.modify (\v -> VM.write v idx (applyTree xs $ RoutingTree appliedNode (rt_children origNode))) (rt_children tree)
            in tree { rt_children = children' }
    in case filter (not . T.null) $ T.splitOn "/" route of
         [] ->
             let currNode = rt_node currTree
                 currNode' = currNode { rd_data = mergeData (rd_data currNode) (Just dat) }
             in currTree { rt_node = currNode' }
         xs -> applyTree (map parseRouteNode xs) currTree

parseRouteNode :: T.Text -> RouteNode
parseRouteNode node =
    case T.uncons node of
      Just (':', var) ->
          RouteNodeCapture $ CaptureVar var
      Just ('{', rest) ->
          case T.uncons (T.reverse rest) of
            Just ('}', def) ->
                let (var, xs) = T.breakOn ":" (T.reverse def)
                in case T.uncons xs of
                     Just (':', regex) ->
                         RouteNodeRegex (CaptureVar var) (buildRegex regex)
                     _ ->
                         nodeError
            _ -> nodeError
      Just _ ->
          RouteNodeText node
      Nothing ->
          nodeError
    where
      nodeError = error ("Spock route error: " ++ (show node) ++ " is not a valid route node.")

emptyParamMap :: ParamMap
emptyParamMap = HM.empty

matchRoute :: T.Text -> RoutingTree a -> [(ParamMap, a)]
matchRoute route globalTree =
    matchRoute' (T.splitOn "/" route) globalTree

matchRoute' :: [T.Text] -> RoutingTree a -> [(ParamMap, a)]
matchRoute' routeParts globalTree =
    case filter (not . T.null) routeParts of
      [] ->
          case rd_data $ rt_node globalTree of
            Nothing -> []
            Just action -> [(emptyParamMap, action)]
      xs ->
          let (_, matches) = findRoute xs (rt_children globalTree) emptyParamMap
          in matches
    where
      findRoute :: [T.Text] -> V.Vector (RoutingTree a) -> ParamMap -> (Bool, [(ParamMap, a)])
      findRoute [] trees _ =
          (V.null trees, [])
      findRoute (textNode : xs) trees paramMap =
          let foundPaths = V.foldl' matchTree V.empty trees
          in (V.null foundPaths, V.toList foundPaths)
          where
            matchTree :: V.Vector (ParamMap, a) -> RoutingTree a -> V.Vector (ParamMap, a)
            matchTree vec rt =
                case matchNode textNode (rd_node $ rt_node rt) of
                  (False, _) ->
                      trace ("No match: " ++ show textNode ++ " @ " ++ (show $ rd_node $ rt_node rt)) vec
                  (True, mCapture) ->
                      let paramMap' =
                              case mCapture of
                                Nothing -> paramMap
                                Just (var, value) ->
                                    HM.insert var value paramMap
                          nodeData = rd_data $ rt_node rt
                          nodeChildren = rt_children rt
                      in case xs of
                           [] | isJust nodeData ->
                                  V.snoc vec (paramMap', fromJust nodeData)
                              | otherwise ->
                                  vec
                           _ -> V.fromList $ snd $ findRoute xs (rt_children rt) paramMap'

{-
data RouteNode
   = RouteNodeRegex !CaptureVar !RegexWrapper
   | RouteNodeCapture !CaptureVar
   | RouteNodeText !T.Text
   | RouteNodeRoot
   deriving (Show, Eq)

data RouteData a
   = RouteData
   { rd_node :: !RouteNode
   , rd_data :: Maybe a
   }
   deriving (Show, Eq)

data RoutingTree a
   = RoutingTree
   { rt_node :: !(RouteData a)
   , rt_children :: !(V.Vector (RoutingTree a))
   }
   deriving (Show, Eq)
-}

matchNode :: T.Text -> RouteNode -> (Bool, Maybe (CaptureVar, T.Text))
matchNode _ RouteNodeRoot = (False, Nothing)
matchNode t (RouteNodeText m) = (m == t, Nothing)
matchNode t (RouteNodeCapture var) = (True, Just (var, t))
matchNode t (RouteNodeRegex var regex) =
    case Regex.matchRegex (rw_regex regex) (T.unpack t) of
      Nothing -> (False, Nothing)
      Just _ -> (True, Just (var, t))
