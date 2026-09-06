# Typed Haskell browser clients

Version 0.15 uses GHC's JavaScript backend and browser Fetch. The package keeps
its historical name, but no longer requires the separate GHCJS compiler.
It supports `Spock-api` 0.17 and all five endpoint methods: GET, POST, PUT,
PATCH and DELETE, including typed path extensions, query parameters and headers.

Use the [shared/server/client example](../examples/browser/README.md) for the
pinned compiler installation, development loop, Chromium tests and deployment.
The protocol module also compiles with native GHC for testing/custom transports;
`Web.Spock.Api.Client.Browser` is exposed only when targeting JavaScript.

```haskell
import Web.Spock.Api.Client
import Web.Spock.Api.Client.Browser
import Shared

-- In IO, using the endpoints defined in examples/browser/shared:
case browserClient defaultClientConfig of
  Left err -> print err
  Right client -> do
    note <- callEndpoint client getNote
    print note
```

`callEndpoint` arguments follow the path captures, then the JSON body (if any).
`callDocumentedEndpoint` inserts declared query/header arguments between those
two groups. Optional `Nothing` values are omitted; query lists use repeated keys
in their original order. Paths/query values are percent encoded. Typed headers
use `ToHttpApiData.toHeader`; additional `(Text, Text)` headers use UTF-8.

For cookie-authenticated writes, obtain the server's CSRF token and pass it
explicitly, for example `callEndpoint' client createNote [("X-CSRF-Token", token)] value`.
The server must validate it; the example uses `prehook csrfCheck` for unsafe API
routes. Cookies default to same-origin Fetch credentials. Cross-origin APIs need
an appropriate server CORS policy; cross-origin cookies also need
`IncludeCredentials` and compatible cookie settings. Fetch controls forbidden
headers such as `Cookie` and `Host`; do not attempt to supply them yourself.

`ClientConfig` selects a base URL/path prefix, extra headers, credentials, slash
policy, timeout and response byte limit. Defaults are same-origin, 30 seconds
and 1 MiB. The browser transport counts streamed bytes and aborts oversized or
timed-out responses. Redirects are rejected. Duplicate header names (case
insensitive), control bytes and unsafe URL prefixes fail before a request.

Results are `Either ClientError a`. Any 2xx response must contain JSON matching
the endpoint's result type; empty 204 bodies produce `DecodeFailure`. Other
statuses yield `HttpError status`. Timeout, network failure and response size
errors have distinct constructors. Errors never include server error bodies,
request payloads or credentials. A custom `Transport` supplied to `newClient`
must implement its timeout; the common decoder also checks response size.
Haskell exceptions from custom transports propagate normally.

## Migration from 0.14

Replace `stack-ghcjs.yaml` with `cabal.project.javascript`; install the pinned
toolchain using the example guide. Construct an explicit `Client` and handle
`Either ClientError` results. Use `browserClient` for browser I/O. Shared
parameter types need both `FromHttpApiData` and `ToHttpApiData` instances in
`Spock-api` 0.17 so the same declaration can parse and encode values.
