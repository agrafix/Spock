Spock
=====

[![Build Status](https://travis-ci.org/agrafix/Spock.svg)](https://travis-ci.org/agrafix/Spock)
[![Hackage](https://img.shields.io/hackage/v/Spock.svg)](http://hackage.haskell.org/package/Spock)

Documentation: [Spock 0.12.0.0](https://spockdocs.s3.eu-central-1.amazonaws.com/Spock-0.12.0.0/Web-Spock.html)

## Intro

Hackage: [Spock](http://hackage.haskell.org/package/Spock)
Stackage: [Spock](https://www.stackage.org/package/Spock)

Another Haskell web framework for rapid development. To get started with Spock, check our [tutorial](https://www.spock.li/tutorial/)
or take a quick look at our example projects.

For more information please visit our homepage at [www.spock.li](https://www.spock.li)

## Session modes (0.15)

Full Spock supports on-demand sessions while retaining database pooling and
application state:

```haskell
cfg <- defaultSpockCfg initialSession database initialState
let sessions = (spc_sessionCfg cfg) { sc_sessionMode = SessionsOnDemand }
spock (cfg { spc_sessionCfg = sessions }) routes
```

`SessionsAlways` remains the default. `SessionsOnDemand` loads, renews, or creates
a session only when a session action or CSRF check needs it; unused requests do
not set session cookies. `SessionsDisabled` bypasses session middleware and
housekeeping entirely. Session actions then raise `SessionUseWhenDisabled`.
Combining disabled sessions with CSRF protection raises `CsrfRequiresSessions`
at startup. CSRF protection works normally with on-demand sessions.

This release adds `sc_sessionMode` to `SessionCfg`; applications constructing the
record directly must supply it. Prefer updating `defaultSessionCfg`.

## Browser sessions and logout

`defaultBrowserSpockCfg` enables CSRF protection and on-demand sessions with
Secure, HttpOnly, SameSite=Lax cookies that expire when the browser session ends.
It is intended for HTTPS, including HTTPS terminated by a reverse proxy. For
local HTTP development, override `cs_secure` to `False` explicitly.

```haskell
cfg <- defaultBrowserSpockCfg initialSession database initialState
spock cfg $ do
  get "csrf" $ getCsrfToken >>= text
  post "logout" $ sessionDestroy >> text "Logged out"
```

Send the token from `/csrf` in `X-Csrf-Token` on logout and other unsafe requests.
`sessionDestroy` atomically revokes the current server session and expires its
cookie with the configured path/domain. Other users remain signed in. A later
session action in the same request creates a fresh empty session. Regeneration
preserves data and replaces the ID atomically; call `sessionRegenerateId` when
logging a user in. Each response sends the final session cookie once.

`CookieSettings` now has `cs_sameSite :: Maybe SameSite`, with `SameSiteLax`,
`SameSiteStrict`, and `SameSiteNone`. The ordinary default remains `Nothing`.
Use `cs_secure = True` with `SameSiteNone`, as browsers require it. Code constructing
`CookieSettings` or `SessionManager` directly must supply the new fields.
