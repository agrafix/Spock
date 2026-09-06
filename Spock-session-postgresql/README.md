# PostgreSQL sessions

This optional package stores Spock session data as JSONB, together with the
session ID, CSRF token, and expiry. Workers share sessions by using the same
database and namespace. Session data needs `ToJSON` and `FromJSON` instances.
Use a separate namespace for each application or incompatible session schema.

```haskell
withResource pool initializePostgresqlSessions -- once before starting workers
store <- newPostgresqlSessionStore
  (defaultPostgresqlSessionCfg { psc_namespace = "my-app" }) pool
cfg <- defaultBrowserSpockCfg emptySession database initialState
let sessions = (spc_sessionCfg cfg) { sc_backend = ServerSessions $ defaultServerSessionCfg $ SessionStoreInstance store }
runSpock 8080 $ spock (cfg { spc_sessionCfg = sessions }) routes
```

The caller owns the `resource-pool` pool of `postgresql-simple` connections.
Schema initialization creates `spock_sessions`; run it during deployment or
once before starting concurrent workers. Ordinary operations use parameterized
queries scoped to the namespace. Store data is plaintext in PostgreSQL, so use
your normal database access controls, transport security, and backup policy.

Each **entire** `ss_runTx` action runs at PostgreSQL SERIALIZABLE isolation.
Serialization conflicts and deadlocks retry up to `psc_maxRetries` (default 20),
with bounded delay. Other errors and exhausted retries propagate to the caller;
failed transactions roll back. `PostgresqlTx` is abstract and has no `MonadIO`
instance, so session transformations cannot accidentally repeat external IO.
Malformed or incompatible JSON raises `SessionDecodeError` without including
session contents in the error. See PostgreSQL's
[transaction isolation documentation](https://www.postgresql.org/docs/17/transaction-iso.html).

Expiry, renewal, and logout use Spock's session manager. Housekeeping, mapping,
and filtering read all sessions in the namespace; tune the housekeeping interval
for your workload. This adapter does not promise unbounded scalability for those
bulk operations. Use explicit JSON migrations or a new namespace when changing
session formats. The initial table schema is version 1; no existing tables or
application data are dropped by initialization.

## Build and run

Install PostgreSQL client development libraries (`libpq-dev` on Debian/Ubuntu,
or PostgreSQL via Homebrew), with `pg_config` on `PATH`.
The separate project keeps libpq optional for ordinary Spock builds:

The project configurations narrowly relax `postgresql-simple`'s older dependency
bounds for GHC 9.14; they do not enable unrestricted dependency relaxation.

```sh
cabal build all --project-file=cabal.project.postgresql
# Set PGHOST, PGPORT, PGUSER and PGDATABASE for your database first.
cabal run spock-postgresql-example --project-file=cabal.project.postgresql
```

The example is a CSRF-protected persistent counter over local HTTP. It explicitly
disables Secure cookies for local development; use HTTPS and Secure cookies in
production. Restart it with the same database to retain the count:

```sh
token=$(curl -s -c cookies.txt http://localhost:8080/csrf)
curl -b cookies.txt -c cookies.txt -H "X-Csrf-Token: $token" -X POST http://localhost:8080/increment
curl -b cookies.txt http://localhost:8080/count
curl -b cookies.txt -c cookies.txt -H "X-Csrf-Token: $token" -X POST http://localhost:8080/logout
```

## Integration tests

Tests require a disposable PostgreSQL database and fail explicitly if the
connection string is missing. They use temporary namespaces and clean them up;
they do not drop tables. CI runs them against PostgreSQL 17.

```sh
export SPOCK_TEST_POSTGRESQL='host=localhost dbname=spock_test user=postgres'
cabal test Spock-session-postgresql --project-file=cabal.project.postgresql --test-show-details=direct
stack test Spock-session-postgresql --stack-yaml stack-postgresql.yaml --system-ghc --no-install-ghc
```

The tests cover persistence across connections, concurrent atomic updates,
renewal versus deletion, namespace isolation, rollback, corrupt data, expiration,
regeneration, and logout against the real PostgreSQL server.
