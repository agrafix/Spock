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
