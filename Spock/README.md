Spock
=====

[![Build Status](https://travis-ci.org/agrafix/Spock.svg)](https://travis-ci.org/agrafix/Spock)
[![Hackage](https://img.shields.io/hackage/v/Spock.svg)](http://hackage.haskell.org/package/Spock)

Documentation: [current API reference](https://www.spock.li/reference/).
Request parsing and response helpers such as `param`, `jsonBody`, `body`, and
`setHeader` are documented in **Web.Spock.Action** (from the Spock-core package)
and reexported by Web.Spock.

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

Since **Spock 0.16**, `SessionsOnDemand` is the default. It loads, renews, or creates
a session only when a session action or CSRF check needs it; unused requests do
not set session cookies. `SessionsDisabled` bypasses session middleware and
housekeeping entirely. Session actions then raise `SessionUseWhenDisabled`.
Combining disabled sessions with CSRF protection raises `CsrfRequiresSessions`
at startup. CSRF protection works normally with on-demand sessions.

`sc_sessionMode` was added to `SessionCfg` in 0.15. Applications constructing the
record directly must supply it. Prefer updating `defaultSessionCfg`.

### Migrating from eager sessions

Previously, every request without a valid session cookie allocated a server-side
session, even for static pages and stateless APIs. Clients that discard cookies
could accumulate one session per request until the one-hour expiry and next
housekeeping sweep. Unused requests now allocate no session and send no session
cookie. Session actions and CSRF checks still create sessions when needed.

If your application relies on a cookie being sent before any session action, or
on visits to stateless routes renewing an existing session, explicitly select
`sc_sessionMode = SessionsAlways`. That mode retains the previous behavior and
requires memory proportional to the number of live sessions. Browser cookie
expiry and the server's `sc_sessionTTL` are separate settings.

The [session soak benchmark](benchmarks/README.md) reproduces cookie-less traffic,
checks expiry cleanup and reports allocations, live heap and session counts.

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

## Slash policies

The default ignores empty path segments for compatibility. Opt into strict
matching or method-preserving canonical redirects; see the
[slash routing guide](https://www.spock.li/tutorials/routing).
