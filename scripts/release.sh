#!/usr/bin/env bash
set -euo pipefail

VERSION="$1"

stack upload reroute
stack upload Spock-core
stack upload Spock
stack upload Spock-api
stack upload Spock-api-server
stack upload Spock-api-ghcjs

git tag "$VERSION"
git push origin --tags
