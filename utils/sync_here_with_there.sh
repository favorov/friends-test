#!/usr/bin/env bash
#
# Replay commits from the Bioconductor copy (there) onto the development tree
# (here), file by file.  See _sync_engine.sh for what "replay" means and for
# the conditions under which nothing happens.
#
#   there = ~/friends.test
#   here  = <this repo>/R/friends.test
#
# Both repositories are used at their currently checked out branch.
# Pass --dry-run to see the plan without writing anything.

set -euo pipefail

UTILS_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
HERE_REPO=$(cd "$UTILS_DIR/.." && pwd)
HERE_PREFIX=R/friends.test
THERE_REPO=$HOME/friends.test
THERE_PREFIX=.

exec "$UTILS_DIR/_sync_engine.sh" \
    "$THERE_REPO" "$THERE_PREFIX" \
    "$HERE_REPO" "$HERE_PREFIX" \
    "$@"
