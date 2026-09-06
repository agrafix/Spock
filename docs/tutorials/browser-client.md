---
layout: page
title: "A shared Haskell browser and server API"
permalink: /tutorials/browser-client
---

The browser example compiles one set of Haskell endpoint declarations for a
native Spock server and GHC's JavaScript backend. Its browser UI creates,
replaces, appends, reloads and deletes a session note using typed Fetch calls.
Another endpoint exercises Unicode paths, `.json` extensions, optional and
repeated query parameters, and typed headers.

Start with the [runnable example and build/deployment guide](https://github.com/agrafix/Spock/tree/master/examples/browser).
It includes a checksummed compiler installer, separate Cabal projects, a local
development loop, a complete deployable asset directory and real Chromium tests.
Native code uses GHC 9.14.1; the pinned JavaScript cross compiler is 9.12.2 with
Emscripten 3.1.74. The historical separate GHCJS/Stack setup has been replaced.

The three packages are deliberately small:

- [Shared](https://github.com/agrafix/Spock/blob/master/examples/browser/shared/src/Shared.hs) defines endpoint methods, paths and JSON types once.
- [Server](https://github.com/agrafix/Spock/blob/master/examples/browser/server/src/BrowserServer.hs) registers handlers and explicitly validates CSRF tokens for unsafe API routes.
- [Client](https://github.com/agrafix/Spock/blob/master/examples/browser/client/src/Main.hs) calls those declarations and renders responses using DOM `textContent`.

The versioned API reference includes
[`callEndpoint`](/reference/Spock-api-ghcjs-0.15.0.1/Web-Spock-Api-Client.html#v:callEndpoint),
[`callDocumentedEndpoint`](/reference/Spock-api-ghcjs-0.15.0.1/Web-Spock-Api-Client.html#v:callDocumentedEndpoint)
and [`browserClient`](/reference/Spock-api-ghcjs-0.15.0.1/Web-Spock-Api-Client-Browser.html#v:browserClient).
These pages are generated from the JavaScript build, including its browser-only module.

The client accepts configurable URL prefixes, credentials, timeouts and response
limits. It returns typed `Either ClientError` results for network, status, size
and decoding failures. Read the [client API and migration guide](https://github.com/agrafix/Spock/tree/master/Spock-api-ghcjs)
before adapting it to an existing application. Custom shared parameter types
need both `FromHttpApiData` and `ToHttpApiData` instances with `Spock-api` 0.17.

For production, serve the native executable and matching static assets behind
TLS on the same origin. The demo's in-memory notes disappear on restart; select
a persistent backend and database when the application needs durable data.
The [browser security guide](/tutorials/security) explains cookie and CSRF setup.

## Client-side routing

[`Spock-browser`](https://github.com/agrafix/Spock/tree/master/Spock-browser)
adds a typed navigation registry and a History API adapter. The demo uses
`route`, `<//>`, `var` and `renderPath` for Home, About and a note path containing
Unicode and an encoded slash. The browser changes its view without reloading
the document and handles back/forward, query strings and fragments.

[`mountRouter`](/reference/Spock-browser-0.1.0.0/Web-Spock-Browser-History.html#v:mountRouter)
owns links inside a configured path prefix; external links, downloads, modified
clicks, alternate targets and same-page fragment links keep their normal browser
behavior. Unknown paths invoke the application's not-found handler. Call
[`unmountRouter`](/reference/Spock-browser-0.1.0.0/Web-Spock-Browser-History.html#v:unmountRouter)
when removing the owning component to detach listeners and release callbacks.

Serve the application shell for GET deep links under the same prefix. The
example reserves `/app/` for that fallback, so refreshing a typed route works
and missing `/api/` routes still return 404. API authorization remains on the
server. The demo's Chromium tests exercise navigation, reloads, native link
behavior and cleanup across repeated mounts.
