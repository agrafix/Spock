{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Routing (htf_thisModulesTests) where

import Test.Framework

import Web.Spock.Routing
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

test_matchNode :: IO ()
test_matchNode =
    do assertEqual (False, Nothing) (matchNode "foo" (RouteNodeRoot))
       assertEqual (True, Just (CaptureVar "x", "123")) (matchNode "123" (RouteNodeCapture (CaptureVar "x")))
       assertEqual (True, Just (CaptureVar "x", "123")) (matchNode "123" (RouteNodeRegex (CaptureVar "x") (buildRegex "^[0-9]+$")))

test_matchRoute :: IO ()
test_matchRoute =
    do assertEqual noMatches (matchRoute "random" routingTree)
       assertEqual (oneMatch emptyParamMap [1]) (matchRoute "/" routingTree)
       assertEqual noMatches (matchRoute "/baz" routingTree)
       assertEqual (oneMatch emptyParamMap [2]) (matchRoute "/bar" routingTree)
       assertEqual (oneMatch (vMap [("baz", "5")]) [3]) (matchRoute "/bar/5" routingTree)
       assertEqual multiMatch (matchRoute "/bar/bingo" routingTree)
       assertEqual (oneMatch (vMap [("baz", "23")]) [4]) (matchRoute "/bar/23/baz" routingTree)
       assertEqual (oneMatch (vMap [("baz", "23"), ("bim", "100")]) [4]) (matchRoute "/bar/23/baz/100" routingTree)
       assertEqual (oneMatch (vMap [("baz", "23"), ("bim", "100")]) [4]) (matchRoute "/ba/23/100" routingTree)
       assertEqual (oneMatch (vMap [("cid", "344"), ("since", "2014-20-14T12:23")]) [6]) (matchRoute "/entry/344/2014-20-14T12:23" routingTree)
       assertEqual (oneMatch (vMap [("cid", "344"), ("since", "2014-20-14T12:23")]) [7]) (matchRoute "/entry/bytags/344/2014-20-14T12:23" routingTree)
       assertEqual multiMatch' (matchRoute "/entry/1/audit" routingTree)
       assertEqual (oneMatch (vMap [("eid", "2"), ("cid", "3")]) [9]) (matchRoute "/entry/2/rel/3" routingTree)
    where
      vMap kv =
          HM.fromList $ map (\(k, v) -> (CaptureVar k, v)) kv
      multiMatch =
          ((oneMatch emptyParamMap [5])
            ++ oneMatch (vMap [("baz", "bingo")]) [3])
      multiMatch' =
          ((oneMatch (vMap [("since", "audit"), ("cid", "1")]) [6])
           ++ (oneMatch (vMap [("eid", "1")]) [8]))
      noMatches = []
      oneMatch pm m = [(pm, m)]
      routingTree =
          foldl (\tree (route, action) -> addToRoutingTree route action tree) emptyRoutingTree routes
      routes =
          [ ("/", [1])
          , ("/bar", [2 :: Int])
          , ("/bar/:baz", [3])
          , ("/bar/bingo", [5])
          , ("/bar/:baz/baz", [4])
          , ("/bar/:baz/baz/:bim", [4])
          , ("/ba/:baz/:bim", [4])
          , ("/entry/:cid/:since", [6])
          , ("/entry/bytags/:cid/:since", [7])
          , ("/entry/:eid/audit", [8])
          , ("/entry/:eid/rel/:cid", [9])
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
