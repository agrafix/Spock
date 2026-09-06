# Comparable HTTP benchmarks

This reproduces the three echo workloads in issues #136 and #161 with current
APIs. Every application runs on Warp, with identical response bodies and two
RTS capabilities. Modes are `warp`, `core`, `default`, `always`, `on-demand`,
`disabled` and `scotty`. The constrained capture accepts ASCII digits only in
every implementation, including the same leading-zero behavior.

The optional project keeps Scotty out of Spock's library dependencies. Scotty
0.30 requires the Form representation from http-api-data 0.6; the benchmark
therefore uses 0.6.3 consistently across all applications, with only that
package's `base` upper bound relaxed for GHC 9.14. Libraries are built with `-O2`.

```sh
cabal build spock-http-bench --project-file=cabal.project.benchmarks
spock_bench_binary="$(cabal list-bin spock-http-bench --project-file=cabal.project.benchmarks)"
node benchmarks/http/run.mjs "$spock_bench_binary" 10000 64 3
```

The Node 22 client uses persistent HTTP connections and discards cookies. It
starts and stops each server itself, validates successful and invalid routes
before timing, validates every measured response, and reports throughput,
p50/p95/p99 latency, errors and timeouts as CSV. A failed response fails the run.
Set `SPOCK_BENCH_PORT` to change the default local port 18083. Optional trailing
mode arguments select a subset. For a quick correctness smoke check:

```sh
node benchmarks/http/run.mjs "$spock_bench_binary" 100 8 1
```

Keep the same hardware, compiler, dependency plan, RTS capabilities, concurrency
and workload when comparing changes. Let other builds finish first. This client
can become the bottleneck; equal throughput at its ceiling does not establish
equal framework performance. For capacity testing, run a separate load generator
on another machine against `spock-http-bench MODE PORT +RTS -N2`.

## Reproduction and fix for #136

The old entropy path called `Crypto.Random.getRandomBytes` for each session ID
and CSRF token, repeatedly probing/opening its entropy backends. Spock and the
request-ID logger now use `System.Entropy.getEntropy`, which uses the operating
system's cryptographic randomness API where available and scoped handles on its
fallback paths. No application PRNG or weaker token format is introduced. See
the [entropy package](https://hackage.haskell.org/package/entropy).

On macOS arm64 with GHC 9.14.1 and Node 22.14.0, three repetitions of 5,000
requests at concurrency 64 gave these median requests/sec:

| Route | Eager before | Eager after | Default after | Scotty after |
| --- | ---: | ---: | ---: | ---: |
| Static | 13,486 | 39,226 | 72,616 | 72,548 |
| Text capture | 13,181 | 38,993 | 72,559 | 71,296 |
| Constrained capture | 9,969 | 25,333 | 77,302 | 72,422 |

Every run completed without errors or timeouts. The large remaining cost in
`always` mode includes allocating and storing a session for every request that
discards its cookie. Spock 0.16 defaults to on-demand sessions, addressing that
unnecessary work for stateless handlers. These results do not establish that
every historical timeout had the same cause; the harness makes further reports
reproducible. CI checks response correctness and successful completion without
enforcing a machine-dependent throughput threshold.
