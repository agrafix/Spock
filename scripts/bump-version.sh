#!/bin/bash
VERSION="$1"

echo "Bumping version to $VERSION ..."

find . -name '*.cabal' -type f -not -path "./reroute/*" -exec sed -i '' "s/^version:.*/version:             $VERSION/g" {} +
