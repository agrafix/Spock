{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Wire (htf_thisModulesTests) where

import Test.Framework

import Web.Spock.Wire

test_combineRoute :: IO ()
test_combineRoute =
    do assertEqual "/foo" ("/" `combineRoute`  "foo")
       assertEqual "/foo" ("" `combineRoute`  "foo")
       assertEqual "/foo" ("/" `combineRoute`  "/foo")
       assertEqual "/foo" ("" `combineRoute`  "/foo")
       assertEqual "/test/foo" ("/test" `combineRoute`  "foo")
       assertEqual "/test" ("/test" `combineRoute`  "")
