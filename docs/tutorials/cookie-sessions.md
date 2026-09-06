---
layout: page
title: "Encrypted cookie sessions"
author: Alexander Thiemann
---

Spock 0.18 can store small session values in an authenticated encrypted cookie.
The optional `Spock-session-cookie` package supplies an XChaCha20-Poly1305 codec
with key rotation. Full Spock keeps its database pool, shared state and CSRF
protection. Its default backend remains the STM server store.

Use `defaultBrowserSpockCfg`, then select
`sc_backend = ClientSessions (defaultClientSessionCfg (cookieSessionCodec keys))`.
Build `keys` with `cookieKey` and `cookieKeyRing`, using a random 32-byte key
from your secret manager and a namespace unique to your application/environment.
Every worker needs the same keys. Your session value needs `ToJSON` and `FromJSON`.
Keep HTTPS, Secure/HttpOnly cookies and CSRF checks enabled.

The [package guide and runnable example](https://github.com/agrafix/Spock/tree/master/Spock-session-cookie)
show the complete setup, deployment key rotation and build commands. The
[API reference](/reference/Spock-session-cookie-0.1.0.0/Web-Spock-Session-Cookie.html)
documents the validated key configuration and codec.

## Choose a backend

| Behavior | Server sessions | Encrypted cookie sessions |
| --- | --- | --- |
| Storage | STM by default; optional PostgreSQL | Browser cookie |
| Per-session read/write/CSRF actions | Supported | Supported |
| Expiry checked by server | Yes | Yes, inside authenticated payload |
| On-demand loading and sliding/fixed TTL | Supported | Supported |
| Immediate individual revocation | Deletes server record | Cannot revoke copied cookies |
| Concurrent modifications | Atomic store transaction | Each request has its own copy |
| Bulk mapping/deletion and sweep hooks | Explicit server capability | Unavailable |
| Size | Backend-dependent | Full Set-Cookie value at most 4096 bytes |

Keep cookie data small, well below 2 KiB after allowing for metadata and
encoding. Oversized updates raise `ClientSessionCookieTooLarge` before changing
the request's saved session or pending cookie. The browser may also limit the
total number of cookies.

`sessionDestroy` expires this browser's cookie. Copies of a stateless cookie
remain usable until their deadline, and a valid cookie can be renewed while
sliding expiry is enabled. Regenerating ID/CSRF values cannot revoke old cookies
either. Use server storage when immediate revocation is required. Concurrent
requests cannot merge their changes: the last response cookie the browser saves
wins. Store coordinated counters and other shared state in a database.

## Migrate server configuration

Move the old `sc_store`, `sc_housekeepingInterval` and `sc_hooks` values into
`ServerSessionCfg` as `ssc_store`, `ssc_housekeepingInterval` and `ssc_hooks`:

```haskell
let server = (defaultServerSessionCfg store)
      { ssc_housekeepingInterval = 600, ssc_hooks = hooks }
    sessions = (spc_sessionCfg cfg) { sc_backend = ServerSessions server }
```

Ordinary session action signatures stay the same. Import
`Web.Spock.SessionActions.Server` for bulk operations. Obtain a handle with
`getServerSessionManager`, which returns `Nothing` for cookie and disabled
backends, or use `requireServerSessionManager` to fail explicitly if unavailable.
Pass that handle to `clearAllSessions` or `mapAllSessions`. These operations
cannot silently become no-ops under a cookie backend.
