# Website and API reference

The website is built from this `docs/` directory by GitHub Pages. Its API
navigation leads to `reference/`, which contains generated, versioned Haddocks
for the native libraries and the JavaScript browser client. `Web.Spock.Action` lives in Spock-core and is linked
from the main Spock module and the reference landing page.

To refresh the published reference after changing a library's public API or
documentation, install the repository's GHC/Cabal toolchain and PostgreSQL's
development library (`libpq-dev` on Ubuntu, `libpq` on Homebrew), then run from
the repository root:

```sh
python3 scripts/setup-javascript.py --prefix "$HOME/.local/share/spock-javascript" --haddock
source "$HOME/.local/share/spock-javascript/env.sh"
python3 scripts/build-reference.py
```

Install Node 22 and the [browser build prerequisites](../examples/browser/README.md)
too. The script uses separate Cabal build directories for seven native libraries
and the two browser packages, `Spock-api-ghcjs` and `Spock-browser`. It links
their exact versions locally and checks the output.
Available dependency documentation links to versioned Hackage pages. Existing
directories for older versions are retained. Review and commit the generated
`docs/reference` files along with the source change; the existing Pages
deployment publishes them with the website. Hackage publication is separate.

The cross bindist omits Haddock. `--haddock` installs the matching 9.12.2 host
Haddock executable, shared libraries and HTML resources from a checksummed
official GHC bindist. Its wrapper points at the JavaScript compiler's settings
and package database. The reference build first compiles current sources with
GHC JavaScript's `-haddock -fwrite-ide-info`, then uses Haddock's
[`--no-compilation` interface mode](https://haskell-haddock.readthedocs.io/latest/invoking.html#avoiding-recompilation).
This includes the JavaScript-only `Web.Spock.Api.Client.Browser` and
`Web.Spock.Browser.History` modules without
attempting native dynamic linking or substituting stub implementations.
The dependency interfaces for reroute and Spock-api are generated first so
`callEndpoint` links to the published shared endpoint types.

Each browser package's `build-info.json` records the documentation's compiler,
target and compatible dependency versions. The link checker requires both
packages' modules, their main entry points, JavaScript provenance and the local `Endpoint` link;
a native-only or missing browser reference fails CI. These website Haddocks
cover the maintained client, not the archived 2016 package releases.

For a local site build, use Ruby 4.0 and Bundler:

```sh
BUNDLE_GEMFILE=docs/Gemfile bundle install
BUNDLE_GEMFILE=docs/Gemfile bundle exec jekyll build --source docs --destination docs/_site
python3 scripts/check-reference.py docs/_site/reference
```

The reference CI builds fresh Haddocks, checks the committed and generated
links, builds the site, and uploads the fresh reference as an artifact.
Internal definition links lead to source pages; modules under `Internal`
remain implementation details and do not promise API stability. To
check the introductory Stack projects and their HTTP responses separately:

```sh
python3 scripts/check-tutorial-setup.py
```
