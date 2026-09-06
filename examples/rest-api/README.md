# Persistent REST example

```sh
cabal run spock-rest-example --project-file=cabal.project.tutorials --builddir=dist-newstyle-tutorials -- api.db 8080
cabal test spock-rest-example --project-file=cabal.project.tutorials --builddir=dist-newstyle-tutorials --test-show-details=direct
stack test spock-rest-example --stack-yaml stack-tutorials.yaml --system-ghc --no-install-ghc
```

The loopback API creates a SQLite database if needed and serves people through
GET, POST, PUT and DELETE. It uses parameterized Persistent operations, explicit
HTTP statuses and JSON errors, and a bracketed one-connection pool. The tests
use disposable database files, including reopening a file to verify persistence.

The tutorial project narrowly relaxes Persistent 2.18's published Aeson and
Template Haskell upper bounds for GHC 9.14. The old `persistent-template`
dependency is unnecessary: `Database.Persist.TH` now belongs to `persistent`.

This is a public stateless API demonstration. Add authentication and authorization
before exposing private data; see [the browser security example](../security)
when using cookies. Review migrations separately before applying schema changes
to an existing production database.
