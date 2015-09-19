Spock
=====

[![Build Status](https://travis-ci.org/agrafix/Spock.svg)](https://travis-ci.org/agrafix/Spock)
[![Hackage](https://img.shields.io/hackage/v/Spock.svg)](http://hackage.haskell.org/package/Spock)

## Intro

Hackage: [Spock](http://hackage.haskell.org/package/Spock)
Stackage: [Spock](https://www.stackage.org/package/Spock)

Another Haskell web framework for rapid development


## Library Usage Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Web.Spock

import qualified Data.Text as T

main =
    runSpock 3000 $ spockT id $
    do get ("echo" <//> var) $ \something ->
        text $ T.concat ["Echo: ", something]

```

For more examples check the examples/ directory.

## Install

* Using cabal: `cabal install Spock`
* Using Stack: `stack install Spock`
* From Source (cabal): `git clone https://github.com/agrafix/Spock.git && cd Spock && cabal install`
* From Source (stack): `git clone https://github.com/agrafix/Spock.git && cd Spock && stack build`

## Features

Another Haskell web framework for rapid development: This toolbox provides
everything you need to get a quick start into web hacking with haskell:

* fast routing (both typesafe and "untyped")
* middleware
* json
* sessions
* cookies
* database helper
* csrf-protection
* typesafe contexts

## Important Links

* [Tutorial](https://www.spock.li/tutorial/)
* [Type-safe routing in Spock](https://www.spock.li/2015/04/19/type-safe_routing.html) 
* [Taking Authentication to the next Level](https://www.spock.li/2015/08/23/taking_authentication_to_the_next_level.html)

### Talks

* English: [Beginning Web Programming in Haskell (using Spock)](https://www.youtube.com/watch?v=GobPiGL9jJ4) (by Ollie Charles)
* German: [Moderne typsichere Web-Entwicklung mit Haskell](https://dl.dropboxusercontent.com/u/15078797/talks/typesafe-webdev-2015.pdf) (by Alexander Thiemann)
* German: [reroute-talk](https://github.com/timjb/reroute-talk) (by Tim Baumann)

## Candy

### Extensions

The following Spock extensions exist:

* Background workers for Spock: [Spock-worker](http://hackage.haskell.org/package/Spock-worker)
* Digestive functors for Spock: [Spock-digestive](http://hackage.haskell.org/package/Spock-digestive)

### Works well with Spock

* User management [users](http://hackage.haskell.org/package/users)
* Data validation [validate-input](http://hackage.haskell.org/package/validate-input)
* Blaze bootstrap helpers [blaze-bootstrap](http://hackage.haskell.org/package/blaze-bootstrap)
* digestive-forms bootstrap helpers [digestive-bootstrap](http://hackage.haskell.org/package/digestive-bootstrap)

### Benchmarks

Please note that these benchmarks might not be up to date anymore.

* https://github.com/philopon/apiary-benchmark
* https://github.com/agrafix/Spock-scotty-benchmark

## Example Projects

* [funblog](https://github.com/agrafix/funblog)
* [makeci](https://github.com/openbrainsrc/makeci)
* [curry-recipes](https://github.com/timjb/reroute-talk/tree/06574561918b50c1809f1e24ec7faeff731fddcf/curry-recipes)

## Notes

Since version 0.7.0.0 Spock supports typesafe routing. If you wish to continue using the untyped version of Spock you can Use `Web.Spock.Simple`. The implementation of the routing is implemented in a separate haskell package called `reroute`.

Since version 0.5.0.0 Spock is no longer built on top of scotty. The
design and interface is still influenced by scotty, but the internal
implementation differs from scotty's.

## Thanks to

* Tim Baumann [Github](https://github.com/timjb) (lot's of help with typesafe routing)
* Tom Nielsen [Github](https://github.com/glutamate)  (much feedback and small improvements)
* ... and all other awesome [contributors](https://github.com/agrafix/Spock/graphs/contributors)!

## Hacking

Pull requests are welcome! Please consider creating an issue beforehand, so we can discuss what you would like to do. Code should be written in a consistent style throughout the project. Avoid whitespace that is sensible to conflicts. (E.g. alignment of `=` signs in functions definitions) Note that by sending a pull request you agree that your contribution can be released under the BSD3 License as part of the `Spock` package or related packages.


## Misc

### Supported GHC Versions

* 7.6.3
* 7.8.4
* 7.10.2

### License

Released under the BSD3 license.
(c) 2013 - 2015 Alexander Thiemann
