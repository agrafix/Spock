# Session retention reproduction

The `session-soak` benchmark reproduces the cookie-less requests from issue #186
without including a network load generator in the measured process. Each batch
serves a fixed `hi` response through the real WAI application. A fresh WAI test
session per request models a client that discards cookies. `reuse` keeps one
cookie jar for the batch and first touches the session explicitly.

```sh
cabal build Spock:bench:session-soak
cabal run Spock:bench:session-soak -- default discard 10000 3
cabal run Spock:bench:session-soak -- always discard 10000 3
cabal run Spock:bench:session-soak -- on-demand discard 10000 3
cabal run Spock:bench:session-soak -- disabled discard 10000 3
cabal run Spock:bench:session-soak -- core discard 10000 3
cabal run Spock:bench:session-soak -- always reuse 10000 3
cabal run Spock:bench:session-soak -- on-demand reuse 10000 3
```

Run configurations sequentially in separate processes. CSV output records
elapsed handler time, allocated bytes since the previous sample, live bytes
after a major GC, session count and open descriptors where supported. The final
row measures the idle application after expiry. The benchmark uses a one-second
TTL and housekeeping interval and fails if sessions survive the subsequent
cleanup window. Production defaults remain a one-hour server TTL and ten-minute
housekeeping interval. It closes its session manager when finished.

On macOS arm64, GHC 9.14.1 and crypton 1.1.5, the first 10,000-request batch gave:

| Configuration | Allocated bytes | Live heap after GC | Stored sessions |
| --- | ---: | ---: | ---: |
| Previous default, eager | 1,427,795,640 | 6,122,664 | 10,000 |
| New default, on-demand | 162,429,064 | 92,840 | 0 |

Three batches reproduced the difference. Eager sessions were reclaimed after
expiry, and descriptor counts stayed at 13 in both configurations: this run
demonstrates unnecessary default session retention, not an unbounded descriptor
leak in the current crypton version. Clients using sessions intentionally still
require server storage; clients discarding their cookies cannot reuse it.

These are local allocation/retention measurements, not portable HTTP throughput
claims. Use the same compiler, RTS settings and dependency plan for comparisons.
Unit tests check the default's behavior without asserting hardware-dependent
timings or heap thresholds.
