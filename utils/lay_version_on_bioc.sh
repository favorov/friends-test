#!/usr/bin/env bash
#
# Lay one version of the package onto the Bioconductor copy as a single commit.
#
#   ./utils/lay_version_on_bioc.sh <commit-ish> [--dry-run]
#
# The commit-ish is resolved in this repository -- a tag such as 0.99.22 is the
# usual thing to pass. Everything the copy tracks is replaced by what that tree
# holds; the files the copy keeps for itself are left alone.
#
# This is not the same tool as _sync_engine.sh and does not replace it:
#
#   _sync_engine.sh  replays per-file commit chains and keeps their author and
#                    date, but matches files by name, so a renaming defeats it.
#   this script      lays down a state. Renames, deletions and additions are
#                    all just content, and git works out for itself which of
#                    them were renames. The price is that everything between
#                    two versions arrives as one commit under your name, so do
#                    not use it to carry somebody else's commits across.
#
# Nothing is committed and nothing is pushed: the result is left staged for
# you to look at.

set -euo pipefail

if [ "$#" -lt 1 ]; then
    echo "usage: $(basename "$0") <commit-ish> [--dry-run]" >&2
    exit 2
fi

WHAT=$1
shift
DRY_RUN=0
if [ "${1:-}" = "--dry-run" ]; then
    DRY_RUN=1
elif [ -n "${1:-}" ]; then
    echo "unknown argument: $1" >&2
    exit 2
fi

UTILS_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
DEV=$(cd "$UTILS_DIR/.." && pwd)
PREFIX=R/friends.test
# Overridable so that the whole thing can be rehearsed on clones before it is
# pointed at the real copy.
BIOC=${FRIENDS_TEST_BIOC:-$HOME/friends.test}

# Files the copy owns and this script must not touch. .github in particular
# holds a workflow that has nothing to do with the package.
KEEP_PATTERN='^\.github/|^\.gitignore$'

# --------------------------------------------------------------------------

die() { echo "error: $*" >&2; exit 2; }

git -C "$DEV" rev-parse --git-dir >/dev/null 2>&1 || die "$DEV is not a git repository"
git -C "$BIOC" rev-parse --git-dir >/dev/null 2>&1 || die "$BIOC is not a git repository"

git -C "$DEV" rev-parse --verify --quiet "$WHAT^{commit}" >/dev/null ||
    die "'$WHAT' does not name a commit in $DEV"

if [ -n "$(git -C "$BIOC" status --porcelain --untracked-files=no)" ]; then
    die "$BIOC has uncommitted changes; commit or stash them first"
fi

VERSION=$(git -C "$DEV" show "$WHAT:$PREFIX/DESCRIPTION" | sed -n 's/^Version: //p')
BRANCH=$(git -C "$BIOC" branch --show-current)
: "${BRANCH:=<detached HEAD>}"

echo "source : $DEV at $WHAT (package version $VERSION)"
echo "target : $BIOC [$BRANCH]"
echo

if [ "$DRY_RUN" -eq 1 ]; then
    echo "files the target tracks now : $(git -C "$BIOC" ls-files | grep -Ecv "$KEEP_PATTERN")"
    echo "files the source would lay  : $(git -C "$DEV" ls-tree -r --name-only "$WHAT" "$PREFIX" | wc -l | tr -d ' ')"
    echo "kept untouched              : $(git -C "$BIOC" ls-files | grep -Ec "$KEEP_PATTERN")"
    echo
    echo "--dry-run: stopping before touching $BIOC"
    exit 0
fi

# Remove what the copy tracks, except the files that are its own.
git -C "$BIOC" ls-files | grep -Ev "$KEEP_PATTERN" | while IFS= read -r f; do
    rm -f "$BIOC/$f"
done
find "$BIOC" -mindepth 1 -type d -empty \
    -not -path "$BIOC/.git/*" -not -path "$BIOC/.git" -delete 2>/dev/null || true

# Lay down the tree.
git -C "$DEV" archive "$WHAT" "$PREFIX" | tar -x -C "$BIOC" --strip-components=2

# Rebuild the index from the working tree. A plain "git add -A" is not enough
# on a case-insensitive filesystem: with core.ignorecase set, git keeps the old
# index entry for a path that differs only in case, so biocparallel-utils.r
# survives beside the .R file that is actually on disk.
git -C "$BIOC" rm -r --cached --quiet .
git -C "$BIOC" add -A

echo "staged in $BIOC:"
git -C "$BIOC" diff --cached --name-status -M | awk '{print substr($1, 1, 1)}' |
    sort | uniq -c | while read -r n kind; do
        case "$kind" in
            A) echo "  $n added" ;;
            D) echo "  $n deleted" ;;
            M) echo "  $n modified" ;;
            R) echo "  $n renamed" ;;
            *) echo "  $n $kind" ;;
        esac
    done

echo
echo "nothing committed. Check it, then:"
echo "    git -C $BIOC commit -m '$VERSION'"
