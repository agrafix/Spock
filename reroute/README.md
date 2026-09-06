reroute
=====

[![Build Status](https://travis-ci.org/agrafix/Spock.svg)](https://travis-ci.org/agrafix/Spock)

[![Hackage Deps](https://img.shields.io/hackage-deps/v/reroute.svg)](http://packdeps.haskellers.com/reverse/reroute)

# Intro

Hackage: http://hackage.haskell.org/package/reroute

An abstract implementation of typesafe and untyped routing for web applications. The web framework
[Spock](https://github.com/agrafix/Spock) is implemented with it. The basic idea is you have a
registry storing a mapping betwenn abstract routes and actions. Then you define two methods for
adding a route and it's action to the registry and a second method for efficiently matching a
provided path to a route and multiple actions.

# Install

* Using cabal: `cabal install reroute`
* From Source: `git clone https://github.com/agrafix/Spock.git && cd Spock/reroute && cabal install`

## Slash policies

The default ignores empty path segments for compatibility. Opt into strict
matching or method-preserving canonical redirects; see the
[slash routing guide](https://www.spock.li/tutorials/routing).

Typed file extensions use `<.>`: `var <.> "txt"` or `var <.> var`.
Use `renderRouteEncoded` for percent-encoded links. The
[routing guide](https://www.spock.li/tutorials/routing#file-extensions-in-typed-routes)
covers matching precedence, multiple dots, and typed extension values.
