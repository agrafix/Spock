# Spock-session-cookie

Store small JSON session values in authenticated encrypted browser cookies,
without a server session store or housekeeping thread. This optional package
uses Crypton's XChaCha20-Poly1305 with a fresh 24-byte OS-random nonce for each
cookie. The session ID, CSRF token, expiry and data are encrypted and
authenticated together. The application namespace, cookie name, wire version
and key ID are authenticated as associated data.

```haskell
import Web.Spock.Config
import Web.Spock.Session.Cookie

-- keyBytes comes from your secret manager: exactly 32 random bytes.
key <- either (fail . show) pure $ cookieKey "2026-09" keyBytes
keys <- either (fail . show) pure $ cookieKeyRing "my-app/production" key []
cfg <- defaultBrowserSpockCfg emptySession database initialState
let sessions = (spc_sessionCfg cfg)
      { sc_backend = ClientSessions $ defaultClientSessionCfg $ cookieSessionCodec keys
      , sc_sessionTTL = 900
      }
spock (cfg { spc_sessionCfg = sessions }) routes
```

Your session value needs `ToJSON` and `FromJSON`. The ordinary `readSession`,
`writeSession`, `modifySession`, `getSessionId`, `getCsrfToken`,
`sessionRegenerateId` and `sessionDestroy` actions work with either backend.
Full Spock's database pooling, state and CSRF checks remain available.
The default remains server-backed, on-demand sessions.

Use HTTPS and keep Secure, HttpOnly and SameSite attributes from
`defaultBrowserSpockCfg`. CSRF checks still matter for encrypted cookies:
browsers attach them automatically. Never expose keys to browser JavaScript,
commit keys, include them in logs, or derive them directly from passwords.
Keep the same keys on all application workers and across restarts. The library
does not create, persist or print application keys. Use separate namespaces
and preferably separate keys for different applications and environments.

## Lifetime, size and concurrency

Spock checks the authenticated deadline against the server clock before using
a cookie. With `sc_sessionExpandTTL = True`, each request that uses a valid
session renews it and issues a new cookie. `False` keeps a fixed deadline.
`SessionsOnDemand` does not decode or renew cookies on unused routes;
`SessionsAlways` loads them on every request. Disabled sessions remain disabled.
Cookie browser lifetime (`cs_EOL`) is separate from the server-checked deadline.

The entire Set-Cookie value, including name and attributes, must fit within
`csc_maxCookieBytes` (default and maximum 4096). Encryption, base64 and metadata
consume part of that budget; keep application data well below 2 KiB. Spock
checks before committing a change. An oversized write raises
`ClientSessionCookieTooLarge` and leaves the last successful state and pending
cookie intact. An uncaught error produces an error response. Browser limits
vary, including limits on the total number of cookies, so test your deployment.
Untrusted data is authenticated before JSON decoding; malformed, expired or
tampered cookies become empty sessions on first use.

Each request modifies its own copy. Simultaneous requests starting with the
same cookie do not share an atomic transaction: whichever response cookie the
browser saves last wins. Avoid using these sessions for counters, balances or
other state that requires coordinated updates. The example serializes its UI
requests; applications needing concurrent updates should use server storage.

`sessionDestroy` expires the current browser cookie, and a later session action
creates a fresh empty session. It cannot revoke a copied cookie. Regenerating an
ID and CSRF token also cannot revoke old stateless cookies. A copied cookie
remains usable until its authenticated expiry and can be renewed while valid.
Use short lifetimes and server storage when immediate per-user revocation is
required. `Web.Spock.SessionActions.Server` deliberately requires an explicit
server capability for `mapAllSessions` and `clearAllSessions`; cookie backends
return `Nothing` from `getServerSessionManager`.

## Rotate keys

Construct `cookieKeyRing namespace newPrimary [previousKey]`, deploy that ring
consistently, and keep the previous key accepted until the last cookie issued
under it has expired. Requests presenting old-key cookies are reissued with the
primary key, even with fixed expiry; this does not extend that fixed deadline.
Remove the old key after the overlap. Up to seven old keys are accepted.
Removing a key immediately rejects cookies issued under it, including visitors
who have not yet received a replacement. Changing the namespace also rejects
existing cookies. Plan deployments and session schema migrations together.

## Run and test

From the repository root:

```sh
cabal build all --project-file=cabal.project.cookie --builddir=dist-newstyle-cookie
cabal test Spock-session-cookie --project-file=cabal.project.cookie \
  --builddir=dist-newstyle-cookie --test-show-details=direct

export SPOCK_COOKIE_KEY="$(openssl rand -base64 32)"
cabal run spock-cookie-example --project-file=cabal.project.cookie \
  --builddir=dist-newstyle-cookie -- --local-http 8080
```

Open <http://127.0.0.1:8080>. Increment, refresh, and reset the counter. Restarting
the server with the same environment key preserves unexpired cookies. Starting
with a different key resets them. The executable binds to loopback, requires an
explicit `--local-http` flag for development, and supports `--https` behind a
trusted TLS proxy. The counter demonstrates session storage, not authentication.

Stack uses `stack --stack-yaml stack-cookie.yaml test --system-ghc
--no-install-ghc --lock-file=error-on-write`. The optional package keeps the
cryptographic dependency out of Spock itself. Tests include cross-instance
requests, CSRF, renewal, key removal, failure atomicity, per-byte tampering,
concurrency semantics and an independently generated libsodium fixture.
Regenerating that public fixture uses Python with PyNaCl 1.6.2 and
`python test/make-vector.py`; ordinary builds and tests do not need Python.
