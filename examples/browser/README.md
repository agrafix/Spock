# One Haskell API, native server and browser client

This example stores a small note in a Spock session. The Haskell client creates,
replaces, appends, reloads and deletes it. A second endpoint demonstrates Unicode
path captures with `.json`, required/optional/repeated query values and headers.

| Package | Purpose | Compiler |
| --- | --- | --- |
| `shared` | Endpoint declarations and JSON types | Both |
| `server` | Spock handlers, CSRF checks and static assets | GHC 9.14.1 |
| `client` | DOM interface and typed Fetch calls | GHC JavaScript 9.12.2 |

The JavaScript compiler is the current prebuilt cross compiler in
[GHCup's cross metadata](https://github.com/haskell/ghcup-metadata/blob/develop/ghcup-cross-0.0.9.yaml).
Its bindist requires Emscripten 3.1.74. The native compiler remains 9.14.1.
Versions, archive SHA-256 hashes and the Emscripten SDK commit are recorded in
[`scripts/javascript-toolchain.json`](../../scripts/javascript-toolchain.json).

## Build and develop

Install native GHC 9.14.1, Cabal 3.18.1.0, Node 22, Python 3.11 or newer,
Git, Make, tar and xz. Linux also needs the usual GHC build prerequisites;
macOS needs Xcode command-line tools. Run from the repository root:

```sh
cabal update
python3 scripts/setup-javascript.py --prefix "$HOME/.local/share/spock-javascript"
source "$HOME/.local/share/spock-javascript/env.sh"
python3 scripts/build-browser.py --test
./dist-browser/spock-browser-server --local-http 8085 "$PWD/dist-browser/public"
```

Open <http://127.0.0.1:8085>. The build script compiles the shared source twice,
runs the same routing/API/client tests under Node, and tests the native server.
Edit `shared/src/Shared.hs`, `server/src/BrowserServer.hs` or `client/src/Main.hs`,
then rerun the build script and reload the page. Restart the server after server
changes. Server and assets share one origin, so no development proxy or CORS
exception is required. Keep the browser URL and host consistent for cookies.

The setup script installs into the selected directory, verifies the download
before extraction and writes `env.sh`; it does not change your shell startup
files. Keep that directory in place: the compiler uses the installed Emscripten.
The cross compiler has a prefixed executable name and does not replace `ghc`.
Use separate Cabal build directories for the two projects, as the script does.

The JavaScript project forces GNU archive format to avoid malformed wasm object
padding in macOS BSD archives. Its Node tests ignore ambient `.hspec` files and
use a fixed QuickCheck seed because GHC 9.12 lacks the filesystem/entropy entry
points those runner features call. Every test still runs. Native tests retain
their ordinary runner configuration. JavaScript routing uses ordered maps for
text keys because the current `hashable` Text instance calls unavailable CApiFFI
symbols; native routing retains hash maps.

To run the actual compiled Haskell client in Chromium:

```sh
npm install --prefix /tmp/spock-browser-tests playwright@1.61.1
/tmp/spock-browser-tests/node_modules/.bin/playwright install chromium
PLAYWRIGHT_MODULE=/tmp/spock-browser-tests/node_modules/playwright \
  node examples/browser/test/browser.cjs \
  "$PWD/dist-browser/spock-browser-server" "$PWD/dist-browser/public"
```

On Linux, use `playwright install --with-deps chromium` to install system
libraries too. CI installs the cross compiler from scratch, runs both Haskell
test targets and Chromium, and uploads `dist-browser` as a deployment artifact.
Chromium covers all five methods, exact Unicode round trips, typed parameters,
session cookies, rejected missing CSRF tokens, decoding/HTTP/network failures,
timeouts, response limits and recovery after failure.

## Shared definitions and calls

`Shared` defines `getNote`, `createNote`, `replaceNote`, `appendNote`, `deleteNote`
and the documented `echo` endpoint. The server registers them with `defEndpoint`
or `defDocumentedEndpoint`; the browser calls those very same declarations.
For example, after constructing `client` and obtaining `token`:

```haskell
callEndpoint client getNote
callEndpoint' client createNote [("X-CSRF-Token", token)] "Hello"
callDocumentedEndpoint client echo "report/a.b λ😀" "a+b&λ"
  (Just 2) ["first", "two words"] "browser" (Just "optional")
```

Calls return `Either ClientError` with the declared result type. Render server
text using DOM `textContent`, as the example does. See the
[client contract and migration guide](../../Spock-api-ghcjs/README.md) for limits,
URL prefixes, credentials and error handling.

## Deploy

Build on the same OS/architecture as the deployment host. Copy `dist-browser`
as one release directory, including the server binary and every file in `public`.
Native shared libraries (for example libc/GMP on Linux) must be available on the
host. Only the native server and static assets are required at runtime; Node,
GHC and Emscripten are build tools.

Run the server behind a TLS reverse proxy on the same origin:

```sh
./spock-browser-server --https 8085 /srv/spock-browser/current/public
```

The executable binds to loopback. Configure the proxy to forward the site's
root, assets and `/api/` to `127.0.0.1:8085`, preserving cookies and headers.
`--https` enables Secure cookies; it describes the external connection and does
not make Warp terminate TLS. `--local-http` is the explicit development mode.
The asset HTML and session reads are not cached. Deploy the binary and assets
together so shared endpoint versions remain compatible.

This is a session demo, with no login system. Notes live in memory, are isolated
per session and disappear when the server restarts. Use a persistent session
backend and application database for durable data or multiple server workers.
