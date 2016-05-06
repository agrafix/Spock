{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Web.Routing.TextRouting where

import Web.Routing.AbstractRouter

import Data.String
import Control.DeepSeq (NFData (..))
import qualified Data.Core.Graph as G
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Mutable as VM
import qualified Text.Regex as Regex

-- | Combine two routes, ensuring that the slashes don't get messed up
combineRoute :: T.Text -> T.Text -> T.Text
combineRoute r1 r2 =
    case T.uncons r1 of
      Nothing -> T.concat ["/", r2']
      Just ('/', _) -> T.concat [r1', r2']
      Just _ -> T.concat ["/", r1', r2']
    where
      r1' =
          if T.last r1 == '/'
          then r1
          else if T.null r2
               then r1
               else T.concat [r1, "/"]
      r2' =
          if T.null r2
          then ""
          else if T.head r2 == '/' then T.drop 1 r2 else r2

type TextAction m r = TAction m r '[]

newtype TPath (a :: ())
    = TPath { unTPath :: T.Text }
    deriving (Show, Eq, IsString, Read, Ord)

newtype TAction m r (p :: [*])
    = TAction (m r)

newtype TActionAppl m r
    = TActionAppl (m r)

data TextRouter (m :: * -> *) a = TextRouter

instance AbstractRouter (TextRouter m a) where
    newtype Registry (TextRouter m a) = TextRouterRegistry (RoutingTree (m a), [[T.Text] -> m a])
    newtype RoutePath (TextRouter m a) xs = TextRouterPath T.Text
    type RouteAction (TextRouter m a) = TAction m a
    type RouteAppliedAction (TextRouter m a) = m a
    subcompCombine (TextRouterPath p1) (TextRouterPath p2) =
        TextRouterPath $ combineRoute p1 p2
    emptyRegistry = TextRouterRegistry (emptyRoutingTree, [])
    rootPath = TextRouterPath "/"
    defRoute (TextRouterPath p) (TAction a) (TextRouterRegistry (tree, cAll)) =
        TextRouterRegistry
        ( addToRoutingTree p a tree
        , cAll
        )
    fallbackRoute routeDef (TextRouterRegistry (m, cAll)) =
        TextRouterRegistry (m, cAll ++ [routeDef])
    matchRoute (TextRouterRegistry (tree, cAll)) path =
        let matches = matchRoute' path tree
        in if null matches
           then matches ++ ((zip (replicate (length cAll) HM.empty) $ map (\f -> f path) cAll))
           else matches

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

instance NFData RegexWrapper where
  rnf (RegexWrapper _ t) = rnf t

data RouteNode
   = RouteNodeRegex !CaptureVar !RegexWrapper
   | RouteNodeCapture !CaptureVar
   | RouteNodeText !T.Text
   | RouteNodeRoot
   deriving (Show, Eq)

instance NFData RouteNode where
  rnf (RouteNodeRegex v w) = rnf v `seq` rnf w
  rnf (RouteNodeCapture v) = rnf v
  rnf (RouteNodeText t) = rnf t
  rnf RouteNodeRoot = ()

data RouteData a
   = RouteData
   { rd_node :: !RouteNode
   , rd_data :: !(V.Vector a)
   }
   deriving (Show, Eq)

instance NFData a => NFData (RouteData a) where
  rnf (RouteData n d) = rnf n `seq` rnf d

data RoutingTree a
   = RoutingTree
   { rm_graph :: G.Graph
   , rm_nodeManager :: V.Vector (RouteData a)
   , rm_rootNode :: G.Node
   } deriving (Show, Eq)

instance NFData a => NFData (RoutingTree a) where
  rnf (RoutingTree g v r) = rnf g `seq` rnf v `seq` rnf r

emptyRoutingTree :: RoutingTree a
emptyRoutingTree =
    let rootNode = 0
        nodeManager = V.singleton (RouteData RouteNodeRoot V.empty)
    in RoutingTree (G.addNode rootNode G.empty) nodeManager rootNode

spawnNode :: G.Node -> RouteData a -> RoutingTree a -> (G.Node, RoutingTree a)
spawnNode parent nodeData rm =
    let nm' = V.snoc (rm_nodeManager rm) nodeData
        nodeId = (V.length nm') - 1
        g' = G.addNode nodeId (rm_graph rm)
        g'' = G.addEdge parent nodeId g'
    in (nodeId, RoutingTree g'' nm' (rm_rootNode rm))

addActionToNode :: G.Node -> a -> RoutingTree a -> RoutingTree a
addActionToNode nodeId nodeAction rm =
    let routeDataOld = (rm_nodeManager rm) V.! nodeId
        routeDataNew =
            routeDataOld
            { rd_data = V.snoc (rd_data routeDataOld) nodeAction
            }
        nm' = V.modify (\v -> VM.write v nodeId routeDataNew) (rm_nodeManager rm)
    in rm { rm_nodeManager = nm' }

addToRoutingTree :: T.Text -> a -> RoutingTree a -> RoutingTree a
addToRoutingTree route action origRm =
    case chunks of
      [] ->
          addActionToNode (rm_rootNode origRm) action origRm
      _ ->
          treeTraversal (map parseRouteNode chunks) (rm_rootNode origRm) origRm
    where
      chunks = filter (not . T.null) $ T.splitOn "/" route
      treeTraversal [] _ rm = rm
      treeTraversal (node : xs) parentGraphNode rm =
          let graph = rm_graph rm
              children = G.children graph parentGraphNode
              nm = rm_nodeManager rm
              matchingChild =
                  VU.find (\nodeId -> node == rd_node (nm V.! nodeId)) children
          in case matchingChild of
               Just childId ->
                   treeTraversal xs childId (if null xs then addActionToNode childId action rm else rm)
               Nothing ->
                   let (childId, rm') =
                           spawnNode parentGraphNode (RouteData node (if null xs then V.singleton action else V.empty)) rm
                   in treeTraversal xs childId rm'

matchRoute :: T.Text -> RoutingTree a -> [(ParamMap, a)]
matchRoute route globalMap =
    matchRoute' (T.splitOn "/" route) globalMap

matchRoute' :: [T.Text] -> RoutingTree a -> [(ParamMap, a)]
matchRoute' routeParts globalRm =
    findRoute (filter (not . T.null) routeParts) (rm_rootNode globalRm) emptyParamMap []
    where
      globalGraph = rm_graph globalRm
      nodeManager = rm_nodeManager globalRm

      findRoute [] parentId paramMap outMap =
          outMap ++ (V.toList $ V.map (\action -> (paramMap, action)) (rd_data (nodeManager V.! parentId)))
      findRoute (chunk : xs) parentId paramMap outMap =
          let children = G.children globalGraph parentId
          in VU.foldl' (\outV nodeId ->
                           case matchNode chunk (rd_node $ nodeManager V.! nodeId) of
                             (False, _) -> outV
                             (True, mCapture) ->
                                 let paramMap' =
                                         case mCapture of
                                           Nothing -> paramMap
                                           Just (var, val) ->
                                               HM.insert var val paramMap
                                 in (findRoute xs nodeId paramMap' outMap) ++ outV
                      ) [] children

buildRegex :: T.Text -> RegexWrapper
buildRegex t =
    RegexWrapper (Regex.mkRegex $ T.unpack t) t

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

matchNode :: T.Text -> RouteNode -> (Bool, Maybe (CaptureVar, T.Text))
matchNode _ RouteNodeRoot = (False, Nothing)
matchNode t (RouteNodeText m) = (m == t, Nothing)
matchNode t (RouteNodeCapture var) = (True, Just (var, t))
matchNode t (RouteNodeRegex var regex) =
    case Regex.matchRegex (rw_regex regex) (T.unpack t) of
      Nothing -> (False, Nothing)
      Just _ -> (True, Just (var, t))
