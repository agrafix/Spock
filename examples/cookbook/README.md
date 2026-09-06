# Request cookbook

```sh
cabal run spock-cookbook-example -- 8080
cabal test spock-cookbook-example --test-show-details=direct
```

The loopback server demonstrates headers, form and JSON bodies, response
middleware, an explicit CORS origin, repeated file uploads, JSON errors and
structured request logging. It is a public stateless example with sessions
disabled. Use the [browser security example](../security) when introducing
cookie authentication.

`Hello.hs` and `HelloSpec.hs` are also compiled here for the testing tutorial.
The website includes are synchronized from these source files; update them
with `python3 scripts/sync-tutorial-examples.py` at the repository root.
