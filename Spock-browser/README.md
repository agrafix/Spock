# Typed browser routing

`Spock-browser` reuses reroute's path combinators and matching rules for browser
navigation. `Web.Spock.Browser` compiles with native GHC and GHC JavaScript;
`Web.Spock.Browser.History` is exposed only by the JavaScript build.
The [runnable demo](../examples/browser/README.md) includes installation, native
and JavaScript tests, real Chromium navigation tests and a deployment recipe.
See the versioned [routing API](https://www.spock.li/reference/Spock-browser-0.1.0.0/Web-Spock-Browser.html)
and [History adapter](https://www.spock.li/reference/Spock-browser-0.1.0.0/Web-Spock-Browser-History.html).

```haskell
{-# LANGUAGE DataKinds, OverloadedStrings #-}
import qualified Data.Text as T
import Web.Spock.Browser
import Web.Spock.Browser.History

-- Supply your own DOM rendering function.
start :: (T.Text -> IO ()) -> IO (Either NavigationError MountedRouter)
start render = do
  router <- compileRoutes IgnoreSlashes $ do
    route "app" (render "Home")
    route "app/about" (render "About")
    route ("app/note" <//> var) (\name -> render ("Note: " <> name))
  mountRouter
    (BrowserConfig "/app" (const $ render "Not found") (render . T.pack . show)) router
```

Keep the returned `MountedRouter`. `navigate mounted Push href` adds a history
entry; `Replace` replaces the current entry. Both return explicit errors and
queue rendering. Back/forward events render the current URL without creating
new entries. Query strings and fragments are preserved; `currentLocation` reads
them for application-specific parsing. Native `dispatch` executes the first
matching route and reports a miss with `False`. Handler exceptions propagate.

Use `renderPath policy path values` for links. It percent-encodes each complete
segment, including typed extensions. Incoming paths decode once per segment,
so `%2F` stays inside a captured value. Invalid escapes/UTF-8, external URLs,
protocol-relative URLs, raw controls and dot segments that browsers normalize
are rejected. `IgnoreSlashes` ignores empty segments; `StrictSlashes` retains
them. `RedirectTrailingSlashes` uses strict matching in the client: canonical
HTTP redirects remain the server's responsibility.

Only ordinary, unmodified left clicks on same-origin links inside the configured
path scope are intercepted. `/app` does not include `/application`. External
links, other schemes, downloads, non-self targets (including a base target),
modified/middle clicks, already-prevented events, `rel="external"`,
`data-no-router` and same-page fragment links retain native browser behavior.
Unknown in-scope paths run `bc_notFound`. Only one History router may be mounted
in a window at a time; duplicate mounts return `AlreadyMounted`.

Call `unmountRouter` when the owning component is removed. Cleanup removes the
click/popstate/hashchange listeners and releases its Haskell callback exactly
once. It is safe to call repeatedly. Queued navigation is ignored after cleanup;
an already-running handler may finish. Handlers run serially in event order, so
keep rendering short and manage cancellation of long application requests in
your application. A handler may call `navigate` without deadlocking.

The server must return the app shell for GET deep links under the same prefix.
The demo reserves `/app/` for this fallback and `/api/` for real API responses;
unknown API paths still return 404. Assets use absolute URLs so refreshing a
deep route loads the same bundle. Browser routing does not provide server-side
authentication or authorization; protect API handlers on the server.
