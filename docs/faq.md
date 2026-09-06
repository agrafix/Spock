---
layout: page
title: "Frequently asked questions"
permalink: /faq/
---

These answers describe the current repository releases (Spock 0.16 and
Spock-core 0.15). Start with [Getting Started](/tutorials/getting-started),
then use the [API reference](/reference/) to look up individual functions.

## Can I use regular expressions or custom types in routes?

Routes use typed path captures. In `get ("number" <//> var) handler`, a handler
taking `Int` makes `var` parse an integer. A failed parse skips that route; if
nothing else matches, the response is 404. A static route takes precedence
over a capture at the same position. Use `AltVar` for an explicit choice
between capture types, as shown in the [routing guide](/tutorials/routing).

There is no built-in regex route constructor. Define a type with a
[`FromHttpApiData`](https://github.com/fizruk/http-api-data) instance to validate
a decoded path segment. This example uses
[`regex-tdfa`](https://github.com/haskell-hvr/regex-tdfa) for a lowercase slug:

```haskell
newtype Slug = Slug Text

slugPattern :: Regex
slugPattern = makeRegexOpts (defaultCompOpt { multiline = False }) defaultExecOpt
  ("^[a-z][a-z0-9-]*$" :: String)

instance FromHttpApiData Slug where
  parseUrlPiece piece
    | matchTest slugPattern (unpack piece) = Right (Slug piece)
    | otherwise = Left "Expected a lowercase slug"

app = get ("slug" <//> var) $ \(Slug slug) -> text slug
```

Use `OverloadedStrings`, and import `Data.Text (Text, unpack)`,
`Text.Regex.TDFA`, `Web.HttpApiData`, and `Web.Spock`. Add `regex-tdfa` and
`http-api-data` to the executable's dependencies. The expression is constant
and matches the entire segment, including when the URL contains an encoded
newline. You can instead write any pure parser in `parseUrlPiece`; a regex
dependency is optional. Add `ToHttpApiData` to support reverse routing with
`renderRoute`.

Use `"rest" <//> wildcard` to capture the remaining path as `Text`, including
multiple segments. A wildcard must come last. `hookAny` receives all path
segments when a typed route is unsuitable. A match only selects a handler;
it does not grant authorization or make a captured value safe as a file path.

The [complete FAQ example](https://github.com/agrafix/Spock/blob/master/Spock/example/FAQ.hs)
compiles and tests these routes, plus the CSRF examples below. Run it from the
repository with `cabal run spock-faq-example`, then visit `/slug/hello-spock`
or `/form` on `http://localhost:8080`.

## Where does Spock store sessions?

The default store is an in-process STM map. The browser receives an opaque
session ID in the `spockcookie` cookie; session values and CSRF tokens stay
on the server. The map disappears on process restart and is not shared
between separately configured workers.

The optional [PostgreSQL session adapter](https://github.com/agrafix/Spock/tree/master/Spock-session-postgresql)
persists sessions and shares them between workers using the same database
and namespace. A custom `SessionStore` must provide atomic transactions for
the complete `ss_runTx` action, including multi-operation updates.

## Does every request create a session?

Since Spock 0.16, the default is `SessionsOnDemand`. A request loads, creates,
or renews a session only when it calls a session action or performs a CSRF
check. Serving a stateless page does not allocate a session or set its cookie.
Even `readSession` and `getCsrfToken` can create a session.

| Mode | Behavior |
| --- | --- |
| `SessionsOnDemand` | Use sessions when an action needs them; the default |
| `SessionsAlways` | Load or create a session on every request; the pre-0.16 behavior |
| `SessionsDisabled` | No session middleware or housekeeping; session actions fail |

Full Spock still provides database pooling and shared state with sessions
disabled. Spock-core lets you choose your own base monad and does not include
full Spock's session manager. See the [monad guide](/tutorials/monads).

## When do sessions expire?

`sc_sessionTTL` defaults to 3600 seconds. With the default
`sc_sessionExpandTTL = True`, loading a valid session renews its server-side
deadline to at least one hour from that access. In on-demand mode, visits
that do not use a session do not renew it. Set expansion to `False` for a
fixed lifetime from session creation.

Expired sessions are rejected immediately when accessed. The default
`sc_housekeepingInterval` is 600 seconds; its sweep reclaims expired records
that nobody accesses. It is not an extra ten-minute validity window.

Cookie lifetime is a separate browser setting (`sc_cookieSettings.cs_EOL`).
`defaultSpockCfg` uses a long-lived cookie, so a browser may send an ID after
its server session has expired; Spock then creates an empty session if needed.
`defaultBrowserSpockCfg` uses a browser-session cookie. Do not rely on closing
a browser as logout: use a protected `sessionDestroy` action to revoke the
server session and expire its cookie.

## How do sessions and CSRF protection fit together?

CSRF protection compares a token supplied by the caller with the token in
that caller's session. Enable it for cookie-authenticated browser actions
using `defaultBrowserSpockCfg`, or set `spc_csrfProtection = True` explicitly.
The older `defaultSpockCfg` leaves it off. Protection works with on-demand
sessions; combining it with `SessionsDisabled` is a startup error.

Full Spock's route helpers check unsafe methods, including POST, PUT, PATCH,
and DELETE. GET, HEAD, and OPTIONS must not change application state. Core
route helpers, including those used by `Spock-api-server`, need an explicit
CSRF check when they are used with cookie authentication; the configuration
flag alone does not wrap routes registered through another layer.

For a form, obtain `getCsrfToken` while rendering and include it in an input
named `__csrf_token`. POST it as `application/x-www-form-urlencoded` with the
session cookie. For JSON, return the token from a same-origin endpoint and
send it in the `X-Csrf-Token` header with the cookie; putting it inside JSON
does not satisfy the check. The names are configurable through
`spc_csrfPostName` and `spc_csrfHeaderName`. Keep tokens out of URLs and logs.

The runnable example serves both `/form` and `/csrf`. Its `/json` endpoint
accepts a JSON string:

```sh
token=$(curl -s -c cookies.txt http://localhost:8080/csrf)
curl -b cookies.txt -H "X-Csrf-Token: $token" \
  -H 'Content-Type: application/json' -d '"hello"' http://localhost:8080/json
```

Both the token and its session cookie are needed. Missing, incorrect, or
expired-session tokens receive 403. CSRF protection does not authenticate a
user or replace permission checks, HTTPS, or HTML escaping.

## Which browser configuration should I start with?

`defaultBrowserSpockCfg` enables CSRF checks and sets Secure, HttpOnly,
SameSite=Lax cookies. Deploy it behind HTTPS, including when a trusted proxy
terminates TLS. For local HTTP development only, the FAQ executable explicitly
disables the Secure flag. Preserve it in production. Rotate the session ID
with `sessionRegenerateId` when signing in and use a CSRF-protected POST route
calling `sessionDestroy` for logout. See the
[session configuration reference](/reference/Spock-0.16.0.1/Web-Spock-Config.html).
