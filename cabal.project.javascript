packages:
  reroute/*.cabal
  Spock-api/*.cabal
  Spock-api-ghcjs/*.cabal
  examples/browser/shared/*.cabal
  examples/browser/client/*.cabal

with-compiler: javascript-unknown-ghcjs-ghc
with-hc-pkg: javascript-unknown-ghcjs-ghc-pkg

tests: True
benchmarks: False
optimization: 1

-- LLVM ar defaults to BSD archives on macOS. Their in-member padding makes
-- GHC 9.12's JS linker pass malformed wasm objects to emcc. GNU archives retain
-- exact member sizes; apply this to dependencies as well as local packages.
package *
  ar-options: --format=gnu
