{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Spock.Routing
  ( matchRoute, matchRoute'
  , RoutingTree, CaptureVar (..), ParamMap, addToRoutingTree, emptyRoutingTree
  , htf_thisModulesTests
  )
where

import Data.Hashable
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Text.Regex as Regex
import qualified Data.HashMap.Strict as HM

import Test.Framework

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
emptyRoutingTree = RoutingTree (RouteData RouteNodeRoot Nothing) V.empty

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
                  V.cons (applyTree xs $ RoutingTree (RouteData current currentDat) V.empty) (rt_children tree)
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

matchRoute :: T.Text -> RoutingTree a -> Maybe (ParamMap, a)
matchRoute route globalTree =
    matchRoute' (T.splitOn "/" route) globalTree

matchRoute' :: [T.Text] -> RoutingTree a -> Maybe (ParamMap, a)
matchRoute' routeParts globalTree =
  case filter (not . T.null) routeParts of
    [] -> fmap (\d -> (emptyParamMap, d)) $ rd_data $ rt_node globalTree
    xs ->
      case findRoute xs globalTree emptyParamMap of
        (_, Nothing) -> Nothing
        (pmap, Just x) -> Just (pmap, x)
  where
    applyParams :: Maybe (CaptureVar, T.Text) -> ParamMap -> ParamMap
    applyParams Nothing x = x
    applyParams (Just (var, t)) x = HM.insert var t x

    handleChildren xs children pmap =
      V.foldl' (\st@(accumParams, res) child ->
              case res of
                Nothing -> findRoute xs child accumParams
                Just _ -> st
            ) (pmap, Nothing) children

    findRoute :: [T.Text] -> RoutingTree a -> ParamMap -> (ParamMap, Maybe a)
    findRoute [] _ pmap = (pmap, Nothing)
    findRoute xs (RoutingTree (RouteData RouteNodeRoot _) children) pmap =
      handleChildren xs children pmap
    findRoute (x:xs) tree pmap =
      case matchNode x (rd_node $ rt_node tree) of
        (True, params) ->
          let params' = applyParams params pmap
          in case xs of
              [] -> (params', rd_data $ rt_node tree)
              _ -> handleChildren xs (rt_children tree) params'
        (False, _) -> 
          (pmap, Nothing)

matchNode :: T.Text -> RouteNode -> (Bool, Maybe (CaptureVar, T.Text))
matchNode _ RouteNodeRoot = (False, Nothing)
matchNode t (RouteNodeText m) = (m == t, Nothing)
matchNode t (RouteNodeCapture var) = (True, Just (var, t))
matchNode t (RouteNodeRegex var regex) =
  case Regex.matchRegex (rw_regex regex) (T.unpack t) of
    Nothing -> (False, Nothing)
    Just _ -> (True, Just (var, t))

test_matchNode :: IO ()
test_matchNode =
  do assertEqual (False, Nothing) (matchNode "foo" (RouteNodeRoot))
     assertEqual (True, Just (CaptureVar "x", "123")) (matchNode "123" (RouteNodeCapture (CaptureVar "x")))
     assertEqual (True, Just (CaptureVar "x", "123")) (matchNode "123" (RouteNodeRegex (CaptureVar "x") (buildRegex "^[0-9]+$")))

test_matchRoute :: IO ()
test_matchRoute =
  do assertEqual Nothing (matchRoute "random" routingTree)
     assertEqual (Just (emptyParamMap, [1])) (matchRoute "/" routingTree)
     assertEqual Nothing (matchRoute "/baz" routingTree)
     assertEqual (Just (emptyParamMap, [2])) (matchRoute "/bar" routingTree)
     assertEqual (Just (HM.fromList [(CaptureVar "baz", "5")], [3])) (matchRoute "/bar/5" routingTree)
     assertEqual (Just (HM.fromList [(CaptureVar "baz", "5")], [3])) (matchRoute "/bar/5" routingTree)
     assertEqual (Just (HM.fromList [(CaptureVar "baz", "23")], [4])) (matchRoute "/bar/23/baz" routingTree)
  where
    routingTree =
      foldl (\tree (route, action) -> addToRoutingTree route action tree) emptyRoutingTree routes
    routes =
      [ ("/", [1])
      , ("/bar", [2 :: Int])
      , ("/bar/:baz", [3])
      , ("/bar/:baz/baz", [4])
      ]

test_parseRouteNode :: IO ()
test_parseRouteNode =
  do assertEqual (RouteNodeText "foo") (parseRouteNode "foo")
     assertEqual (RouteNodeCapture (CaptureVar "bar")) (parseRouteNode ":bar")
     assertEqual (RouteNodeRegex (CaptureVar "bar") (buildRegex "^[0-9]$")) (parseRouteNode "{bar:^[0-9]$}")

test_addToRoutingTree :: IO ()
test_addToRoutingTree =
  do assertEqual baseRoute (addToRoutingTree "/" [True] emptyT)
     assertEqual (fooBar []) (addToRoutingTree "/foo/:bar" [True] emptyT)
     assertEqual (fooBar baz) (addToRoutingTree "/foo/:bar/baz" [True] (fooBar []))
  where
    emptyT = emptyRoutingTree
    baseRoute = RoutingTree { rt_node = RouteData{rd_node = RouteNodeRoot, rd_data = Just [True]}, rt_children = V.empty}
    baz = [ RoutingTree { rt_node = RouteData { rd_node = RouteNodeText "baz", rd_data = Just [True] },rt_children = V.empty }]
    fooBar xs =
      RoutingTree
      { rt_node =
          RouteData {rd_node = RouteNodeRoot, rd_data = Nothing }
      , rt_children =
          V.fromList
          [ RoutingTree { rt_node = RouteData{rd_node = RouteNodeText "foo", rd_data = Nothing}
                        , rt_children =
                            V.fromList
                            [ RoutingTree { rt_node = RouteData { rd_node = RouteNodeCapture (CaptureVar "bar")
                                                                , rd_data = Just [True]
                                                                }
                                          , rt_children = V.fromList xs}]}]}
