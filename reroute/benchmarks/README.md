To run the benchmarks:

```bash
$ cabal configure --enable-benchmarks --enable-tests
$ cabal build
$ cabal bench --benchmark-options "--output reroute-benchmarks.html"
```

Then open `reroute-benchmarks.html`.