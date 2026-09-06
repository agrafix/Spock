# Website and API reference

The website is built from this `docs/` directory by GitHub Pages. Its API
navigation leads to `reference/`, which contains generated, versioned Haddocks
for the native libraries. `Web.Spock.Action` lives in Spock-core and is linked
from the main Spock module and the reference landing page.

To refresh the published reference after changing a library's public API or
documentation, install the repository's GHC/Cabal toolchain and PostgreSQL's
development library (`libpq-dev` on Ubuntu, `libpq` on Homebrew), then run from
the repository root:

```sh
python3 scripts/build-reference.py
```

The script uses a separate Cabal build directory, builds all six native
libraries together, links their exact versions locally, and checks the output.
Other dependencies link to their versioned Hackage documentation. Existing
directories for older versions are retained. Review and commit the generated
`docs/reference` files along with the source change; the existing Pages
deployment publishes them with the website. Hackage publication is separate.

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
