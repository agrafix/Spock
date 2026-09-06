[Spock](https://www.spock.li)
=====

![Build status](https://github.com/agrafix/Spock/actions/workflows/haskell.yml/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/Spock.svg)](http://hackage.haskell.org/package/Spock)
[![Hackage Spock-Core](https://img.shields.io/hackage/v/Spock-core.svg)](http://hackage.haskell.org/package/Spock-core)

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
* [request IDs and structured logging](Spock-core/README.md)
* json
* sessions
* optional [persistent PostgreSQL sessions](Spock-session-postgresql/README.md)
* cookies
* database helper
* csrf-protection
* typesafe contexts
* [typed JSON APIs and OpenAPI generation](Spock-api/README.md)

## File uploads

Use `filesMulti` to retrieve every uploaded file, grouped by form field name.
For example, an `<input type="file" name="documents" multiple>` can be read with:

```haskell
uploads <- filesMulti
let documents = HM.lookupDefault [] "documents" uploads
```

Here `HM` is `Data.HashMap.Strict`. Each list preserves upload order, and each
`UploadedFile` provides its name, content type, and temporary location. Process or
copy the temporary files during the request; they are removed when it finishes.
The existing `files` function continues to return the last file for each field.

## Important Links

* [Tutorial](https://www.spock.li/tutorial/)
* [REST API Tutorial](https://www.spock.li/tutorials/rest-api)
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
* Lucid for Spock: [Spock-lucid](http://hackage.haskell.org/package/Spock-lucid)

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

* [Tim Baumann](https://github.com/timjb) (lot's of help with typesafe routing)
* [Tom Nielsen](https://github.com/glutamate)  (much feedback and small improvements)
* ... and all other awesome [contributors](https://github.com/agrafix/Spock/graphs/contributors)!

## Hacking

The native packages use GHC 9.14.1. Install it with [GHCup](https://www.haskell.org/ghcup/),
along with Cabal 3.16.0.0 or newer (CI uses 3.18.1.0):

```sh
ghcup install ghc 9.14.1
ghcup set ghc 9.14.1
ghcup install cabal 3.18.1.0
ghcup set cabal 3.18.1.0
cabal update
cabal build all
cabal test all --test-show-details=direct
```

The Cabal project enables all native libraries, tests, and the routing benchmark.
To run the benchmark, use `cabal bench reroute`.

Stack 3.11.1 is also supported. `stack.yaml` pins the package snapshot and overrides
its compiler with GHC 9.14.1; `stack.yaml.lock` records the resolved dependencies:

```sh
stack build --test --bench --no-run-tests --no-run-benchmarks
stack test -j 1
```

The historical `Spock-api-ghcjs` package and `stack-ghcjs.yaml` target the separate
GHCJS compiler. They are excluded from the native build and CI; this GHC upgrade
does not port the browser client to GHC's JavaScript backend.
The historical client is constrained to `Spock-api < 0.15`; the expanded native
API DSL is documented in [Spock-api/README.md](Spock-api/README.md).

Pull requests are welcome! Please consider creating an issue beforehand, so we can discuss what you would like to do. Code should be written in a consistent style throughout the project. Avoid whitespace that is sensible to conflicts. (E.g. alignment of `=` signs in functions definitions)

Note that by sending a pull request you agree that your contribution can be released under the BSD3 License as part of the `Spock` package or related packages.


## Misc

### Officially Supported GHC Versions

* 9.14.1 (Linux, macOS, and Windows CI)

### License

Released under the BSD3 license.
(c) 2013 - 2021 Alexander Thiemann
