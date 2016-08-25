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
