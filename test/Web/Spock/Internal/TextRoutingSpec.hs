{-# LANGUAGE OverloadedStrings #-}
module Web.Spock.Internal.TextRoutingSpec (spec) where

import Test.Hspec

import Web.Spock.Internal.TextRouting
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

spec :: Spec
spec =
    do matchNodeDesc
       matchRouteDesc
       parseRouteNodeDesc
       addToRoutingTreeDesc

matchNodeDesc :: Spec
matchNodeDesc =
    describe "matchNode" $
    do it "shouldn't match to root node" $
          matchNode "foo" RouteNodeRoot `shouldBe` (False, Nothing)
       it "should capture basic variables" $
          matchNode "123" (RouteNodeCapture (CaptureVar "x")) `shouldBe` (True, Just (CaptureVar "x", "123"))
       it "should work with regex" $
          matchNode "123" (RouteNodeRegex (CaptureVar "x") (buildRegex "^[0-9]+$")) `shouldBe` (True, Just (CaptureVar "x", "123"))

matchRouteDesc :: Spec
matchRouteDesc =
    describe "matchRoute" $
    do it "shouldn't match unknown routes" $
          do matchRoute "random" routingTree `shouldBe` noMatches
             matchRoute "/baz" routingTree `shouldBe` noMatches
       it "should match known routes" $
          do matchRoute "/" routingTree `shouldBe` oneMatch emptyParamMap [1]
             matchRoute "/bar" routingTree `shouldBe` oneMatch emptyParamMap [2]
       it "should capture variables in routes" $
          do matchRoute "/bar/5" routingTree `shouldBe` oneMatch (vMap [("baz", "5")]) [3]
             matchRoute "/bar/23/baz" routingTree `shouldBe` oneMatch (vMap [("baz", "23")]) [4]
             matchRoute "/bar/23/baz/100" routingTree `shouldBe` oneMatch (vMap [("baz", "23"), ("bim", "100")]) [4]
             matchRoute "/ba/23/100" routingTree `shouldBe` oneMatch (vMap [("baz", "23"), ("bim", "100")]) [4]
             matchRoute "/entry/344/2014-20-14T12:23" routingTree `shouldBe` oneMatch (vMap [("cid", "344"), ("since", "2014-20-14T12:23")]) [6]
             matchRoute "/entry/bytags/344/2014-20-14T12:23" routingTree `shouldBe` oneMatch (vMap [("cid", "344"), ("since", "2014-20-14T12:23")]) [7]
             matchRoute "/entry/2/rel/3" routingTree `shouldBe` oneMatch (vMap [("eid", "2"), ("cid", "3")]) [9]
       it "should handle multiple possibile matches correctly" $
          do matchRoute "/bar/bingo" routingTree `shouldBe` multiMatch
             matchRoute "/entry/1/audit" routingTree `shouldBe` multiMatch'
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

parseRouteNodeDesc :: Spec
parseRouteNodeDesc =
    describe "parseRouteNode" $
    do it "parses text nodes correctly" $
          parseRouteNode "foo" `shouldBe` RouteNodeText "foo"
       it "parses capture variables" $
          parseRouteNode ":bar" `shouldBe` RouteNodeCapture (CaptureVar "bar")
       it "parses regex capture variables" $
          parseRouteNode "{bar:^[0-9]$}" `shouldBe` RouteNodeRegex (CaptureVar "bar") (buildRegex "^[0-9]$")

addToRoutingTreeDesc :: Spec
addToRoutingTreeDesc =
    describe "addToRoutingTree" $
    do it "adds the root node correctly" $
          addToRoutingTree "/" [True] emptyT `shouldBe` baseRoute
       it "adds a new branch correctly" $
          addToRoutingTree "/foo/:bar" [True] emptyT `shouldBe` fooBar []
       it "add a new subbranch correctly" $
          addToRoutingTree "/foo/:bar/baz" [True] (fooBar []) `shouldBe` fooBar baz
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
