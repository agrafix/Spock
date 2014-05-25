Spock
=====

[![Build Status](https://drone.io/github.com/agrafix/Spock/status.png)](https://drone.io/github.com/agrafix/Spock/latest)

# Intro

Another Haskell web framework based on scotty: This toolbox provides
everything you need to get a quick start into web hacking with haskell:

* routing
* middleware
* json
* blaze
* sessions
* cookies
* database helper
* csrf-protection
* global state

* Hackage: http://hackage.haskell.org/package/Spock


# Install

* Using cabal: `cabal install Spock`
* From Source: `git clone https://github.com/agrafix/Spock.git && cd Spock && cabal install`

# Candy

The following Spock extensions exist:

* Authentification helpers for Spock: http://hackage.haskell.org/package/Spock-auth
* Background workers for Spock: http://hackage.haskell.org/package/Spock-worker

# Examples

* https://github.com/agrafix/funblog
* https://github.com/openbrainsrc/makeci

# Notes

Since version 0.5.0.0 Spock is no longer built on top of scotty. The
design and interface is still influenced by scotty, but the internal
implementation differs from scotty's.
