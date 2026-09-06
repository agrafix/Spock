#!/bin/sh
set -eu
cd "$(dirname "$0")/../.."

# The comparison uses different dependencies from the native project.
# Keep in-place package registrations separate when switching projects.
cabal build spock-http-bench --project-file=cabal.project.benchmarks --builddir=dist-newstyle-benchmarks >&2
spock_bench_binary="$(cabal list-bin spock-http-bench --project-file=cabal.project.benchmarks --builddir=dist-newstyle-benchmarks)"
exec node benchmarks/http/run.mjs "$spock_bench_binary" "$@"
