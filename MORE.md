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

Benchmarks:

* https://github.com/philopon/apiary-benchmark
* https://github.com/agrafix/Spock-scotty-benchmark

## Important Links

* [Tutorial](https://www.spock.li/tutorial/)
* [Type-safe routing in Spock](https://www.spock.li/2015/04/19/type-safe_routing.html) 
* [Beginning Web Programming in Haskell (using Spock)](https://www.youtube.com/watch?v=GobPiGL9jJ4)

### Talks

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

## Example Projects

* [funblog](https://github.com/agrafix/funblog)
* [makeci](https://github.com/openbrainsrc/makeci)
* [curry-recipes](https://github.com/timjb/reroute-talk/tree/06574561918b50c1809f1e24ec7faeff731fddcf/curry-recipes)

## Companies / Projects using Spock

* http://cp-med.com
* http://openbrain.co.uk
* http://findmelike.com
* https://www.tramcloud.net
* http://thitp.de

## Notes

Since version 0.7.0.0 Spock supports typesafe routing. If you wish to continue using the untyped version of Spock you can Use `Web.Spock.Simple`. The implementation of the routing is implemented in a separate haskell package called `reroute`.

Since version 0.5.0.0 Spock is no longer built on top of scotty. The
design and interface is still influenced by scotty, but the internal
implementation differs from scotty's.

## Thanks to

* Tim Baumann [Github](https://github.com/timjb) (lot's of help with typesafe routing)
* Tom Nielsen [Github](https://github.com/glutamate)  (much feedback and small improvements)
