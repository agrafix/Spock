Spock
=====

[![Build Status](https://travis-ci.org/agrafix/Spock.svg)](https://travis-ci.org/agrafix/Spock)
[![Hackage](https://img.shields.io/hackage/v/Spock.svg)](http://hackage.haskell.org/package/Spock)
[![Hackage Spock-Core](https://img.shields.io/hackage/v/Spock-core.svg)](http://hackage.haskell.org/package/Spock-core)

Documentation: [Spock 0.12.0.0](https://spockdocs.s3.eu-central-1.amazonaws.com/Spock-0.12.0.0/Web-Spock.html)

## Intro

Another Haskell web framework for rapid development. To get started with Spock, check our [tutorial](https://www.spock.li/tutorial/)
or take a look at our example project [funblog](https://github.com/agrafix/funblog)!

## Mailing list

Please join our mailing list at haskell-spock@googlegroups.com

## Features

Another Haskell web framework for rapid development: This toolbox provides
everything you need to get a quick start into web hacking with haskell:

* fast typesafe routing
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

* English: [ZuriHac 2016: Spock - Powerful Elegant Web Applications](https://www.youtube.com/watch?v=-b-Oz6y-n_Y) (by Alexander Thiemann)
* English: [Spock - Powerful Elegent Web Applications using Haskell](https://www.youtube.com/watch?v=kNqsOBrCbLo) (by Alexander Thiemann)
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

### SSL / HTTPS

If you'd like to use your application via HTTPS, there are two options:

* Use nginx/haproxy/... as reverse proxy in front of the Spock application.
* Convert the Spock application to a `wai`-application using the `spockAsApp`. Then use the `warp-tls` package to run it.

## Notes

Since version 0.11.0.0 Spock drops simple routing in favor of typesafe routing and drops safe actions in favor of the "usual" way of csrf protection with a token.

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

* 7.10.2
* 8.0

### License

Released under the BSD3 license.
(c) 2013 - 2017 Alexander Thiemann
