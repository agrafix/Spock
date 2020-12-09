---
layout: page
title: Automated Testing
date: 2017-07-03 15:33:06
author: Louis Pilfold
permalink: /tutorials/testing
---

Thanks to Haskell's type system we can avoid most the errors that we might
encounter when using less safe languages, but some automated tests can still
help verify our code is correct. Let's explore how we can write unit tests for
our HTTP service built with Spock.

## Hello world

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config

main :: IO ()
main =
    do spockCfg <- defaultSpockCfg () PCNoDatabase ()
       runSpock 8080 (spock spockCfg app)

app :: SpockM () () () ()
app =
    do get root $
           text "Hello World!"
       get ("hello" <//> var) $ \name ->
           do text ("Hello " <> name)
```

Here is a simple Spock application, similar to the one from the
[Hello World][hw-tut]. It has two routes, `/` which responds with "Hello,
world!", and `/hello/:name`, which responds with "Hello `$NAME`", where name
is the value of the second path segment.

It type checks, so we are confident that the application will run without
crashing, but we might want some tests to check that the business logic has
been correctly implemented. We can do this with a little help from the
[Hspec][hspec] and [Hspec-Wai][hspec-wai] libraries.

Add Hspec and Hspec-Wai to your tests dependencies in your project's
 Cabal file in the test-suite section:

```haskell
-- snip
test-suite app-test
  build-depends:   Spock >=0.14
                 , base >=4.7 && <5
                 , hspec
                 , hspec-wai
-- snip
```

## Creating tests

Now we create a file test/Spec.hs, start with the test setup and add a basic
test.

```haskell
module Spec where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "the universe" $
  it "behaves the way we expect it to" $ do
    1 `shouldBe` 1
```

We can run this with `stack test`, and providing mathematics hasn't changed
since last time we used it we should see something like this printed to the
console:

```
Spec
  the universe
    behaves the way we expect it to

Finished in 0.0050 seconds
1 examples, 0 failures
```

Now that the tests run we can start testing the web application using
Hspec-Wai. This library tests our application as an `IO Application`, where
`Application` is defined in `Network.Wai`. We'll need to restructure our
application slightly to expose this to the tests.

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main (main, app) where

import Web.Spock
import Web.Spock.Config
import Network.Wai (Middleware)

main :: IO ()
main =
    runSpock 8080 app

app :: IO Middleware
app =
    do spockCfg <- defaultSpockCfg () PCNoDatabase ()
       spock spockCfg app

routes :: SpockM () () () ()
routes =
    do get root $
           text "Hello World!"
       get ("hello" <//> var) $ \name ->
           do text ("Hello " <> name)
```

`app` has been renamed to `routes`, and the `main` function has been split
into two. The new `main` is responsible for only the running of the
application, with the new `app` function being responsible for configuring the
application. Through the new `app` we can access an `IO Middleware` which we
can convert into an `IO Application` in the tests using `spockAsApp`.

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Spec where

import Main (app)
import Test.Hspec
import Test.Hspec.Wai
import Web.Spock (spockAsApp)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    with (spockAsApp app) $
        do describe "GET /" $
               do it "serves the home page" $
                      get "/" `shouldRespondWith` "Hello World" {matchStatus = 200}
           describe "GET /hello/:name" $
               do it "returns hello to spock" $
                      get "/hello/spock" `shouldRespondWith` "Hello spock"
                  it "returns hello to uhura" $
                      get "/hello/uhura" `shouldRespondWith` "Hello uhura"
```

The `shouldRespondWith` function is used to make assertions about the status
code, headers and body content of the HTTP response from the Spock
application. `get "/hello/spock" `shouldRespondWith` "Hello spock"` asserts
that a GET request to `/hello/spock` will result in a HTTP response with the
body content "Hello spock".

For more information on testing check out [Hspec-Wai's documentation on
Hackage][hspec-wai-hackage].

[hw-tut]: {{ "/tutorials/getting-started" | prepend: site.baseurl }}
[hspec]: https://github.com/hspec/hspec
[hspec-wai]: https://github.com/hspec/hspec-wai
[hspec-wai-hackage]: https://hackage.haskell.org/package/hspec-wai
