Spock
=====

[![Build Status](https://travis-ci.org/agrafix/Spock.svg)](https://travis-ci.org/agrafix/Spock)

[![Hackage Deps](https://img.shields.io/hackage-deps/v/Spock.svg)](http://packdeps.haskellers.com/reverse/Spock)

# Intro

Hackage: [Spock](http://hackage.haskell.org/package/Spock)

Another Haskell web framework for rapid development: This toolbox provides
everything you need to get a quick start into web hacking with haskell:

* fast routing (both typesafe and "untyped")
* middleware
* json
* blaze
* sessions
* cookies
* database helper
* csrf-protection

Benchmarks:

* https://github.com/philopon/apiary-benchmark
* https://github.com/agrafix/Spock-scotty-benchmark

# Usage (Simple)

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Web.Spock.Simple

import qualified Data.Text as T

main =
    runSpock 3000 $ spockT id $
    do get ("echo" <//> ":something") $
        do Just something <- param "something"
           text $ T.concat ["Echo: ", something]
       get ("regex" <//> "{number:^[0-9]+$}") $
        do Just number <- param "number"
           text $ T.concat ["Just a number: ", number]
```

# Usage (Typesafe)

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Web.Spock.Safe

import qualified Data.Text as T

main =
    runSpock 3000 $ spockT id $
    do get ("echo" <//> var) $ \something ->
        text $ T.concat ["Echo: ", something]
```

# Install

* Using cabal: `cabal install Spock`
* From Source: `git clone https://github.com/agrafix/Spock.git && cd Spock && cabal install`

# Candy

The following Spock extensions exist:

* Background workers for Spock: [Spock-worker](http://hackage.haskell.org/package/Spock-worker)
* Digestive functors for Spock: [Spock-digestive](http://hackage.haskell.org/package/Spock-digestive)

# Example Projects

* http://findmelike.com/
* https://github.com/agrafix/funblog
* https://github.com/openbrainsrc/makeci

# Companies using Spock

* http://cp-med.com/
* http://openbrain.co.uk/
* http://thitp.de

# Notes

Since version 0.7.0.0 Spock supports typesafe routing. If you wish to continue using the untyped version of Spock you can Use `Web.Spock.Simple`. The implementation of the routing is implemented in a separate haskell package called `reroute`.

Since version 0.5.0.0 Spock is no longer built on top of scotty. The
design and interface is still influenced by scotty, but the internal
implementation differs from scotty's.

# Thanks to

* Tim Baumann [Github](https://github.com/timjb) (lot's of help with typesafe routing)
* Tom Nielsen [Github](https://github.com/glutamate)  (much feedback and small improvements)
