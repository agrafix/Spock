To run the benchmarks:

```sh
cabal bench reroute --benchmark-options '--output reroute-benchmarks.html'
```

Then open `reroute-benchmarks.html`.

The cases cover static lookup, fixed and captured extensions, and rejecting a
wrong suffix on a filename containing many dots. Use `--benchmark-options '-n 1'`
for a single-iteration smoke check.
