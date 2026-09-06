---
layout: page
title: "Automated testing"
permalink: /tutorials/testing
---

HTTP tests check response status, headers, bodies, and state changes through
the real WAI application. Types help compose handlers, but runtime exceptions,
incorrect responses, and authorization mistakes still need testing.

The application and specs below are compiled in the repository's
[cookbook example](https://github.com/agrafix/Spock/tree/master/examples/cookbook).
You can run them there with `cabal test spock-cookbook-example`, or create a
small standalone project as follows.

## Project files

Create a directory named `spock-testing-example` with `src`, `app`, and `test`
subdirectories. Put this in `spock-testing-example.cabal`:

<!-- testing:cabal -->
```cabal
cabal-version: 2.0
name: spock-testing-example
version: 0.1.0.0
build-type: Simple

library
  hs-source-dirs: src
  exposed-modules: Hello
  build-depends: base >= 4.12 && < 5, Spock >= 0.16 && < 0.17, wai
  default-language: Haskell2010
  ghc-options: -Wall

executable spock-testing-example
  hs-source-dirs: app
  main-is: Main.hs
  build-depends: base, Spock, spock-testing-example
  default-language: Haskell2010
  ghc-options: -Wall -threaded

test-suite http-tests
  type: exitcode-stdio-1.0
  hs-source-dirs: test
  main-is: Spec.hs
  other-modules: HelloSpec
  build-depends: base, Spock, spock-testing-example, hspec, hspec-wai
  default-language: Haskell2010
  ghc-options: -Wall -threaded
```

Use the same `stack.yaml` as [Getting Started](getting-started):

{% highlight yaml %}
{% include tutorial-stack.yaml %}
{% endhighlight %}

This is a Cabal-only project. If adapting an Hpack project, put its dependencies
in `package.yaml` instead of editing the generated Cabal file.

## Separate configuration from routes

Create `src/Hello.hs`:

{% highlight haskell %}
{% include examples/Hello.hs %}
{% endhighlight %}

`app` builds middleware; `routes` registers handlers. Passing `routes` to
`spock` keeps setup separate from serving requests. Put the executable's entry
point in `app/Main.hs`:

<!-- testing:main -->
```haskell
module Main (main) where

import Hello (app)
import Web.Spock (runSpock)

main :: IO ()
main = runSpock 8080 app
```

## Exercise the WAI application

Create `test/HelloSpec.hs`:

{% highlight haskell %}
{% include examples/HelloSpec.hs %}
{% endhighlight %}

Then create `test/Spec.hs`:

<!-- testing:driver -->
```haskell
module Main (main) where

import qualified HelloSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec HelloSpec.spec
```

`spockAsApp app` converts middleware to an `IO Application`.
`Test.Hspec.Wai.with` builds an application for each example, and
`shouldRespondWith` checks its response. Match the exact body when it matters:
the home page here sends `Hello World!`, including the exclamation mark.
Use `matchHeaders` to assert response headers and `matchStatus` for status codes.

```sh
stack test --fast --pedantic
```

These tests run in process without opening a network port. Commit the generated
`stack.yaml.lock` to retain the resolved dependencies.

## Test failures and separate clients

Add invalid input, missing resources, denied access, and repeated updates to
your tests. The [REST example tests](https://github.com/agrafix/Spock/blob/master/examples/rest-api/test/Spec.hs)
use disposable SQLite databases and check persistence after closing and reopening
the pool. The [security example tests](https://github.com/agrafix/Spock/blob/master/examples/security/test/Spec.hs)
cover tokens, old session IDs, logout, and escaping.

WAI's test client remembers cookies inside a session and prepends them to later
requests. Omitting an explicit `Cookie` header therefore does not necessarily
simulate another visitor. For tests involving distinct clients, use separate
`Network.Wai.Test.runSession` calls against the same application and supply only
the selected client's cookies, or explicitly clear the client jar. The security
example uses separate calls to verify that one visitor's token cannot be used
by another visitor.

See [hspec-wai's documentation](https://github.com/hspec/hspec-wai) for additional
matchers and setup helpers, then continue with the [request cookbook](requests).
