#!/usr/bin/env bash
#
# Engine shared by sync_there_with_here.sh and sync_here_with_there.sh.
#
# The friends.test sources live in two git repositories with unrelated
# histories: the development tree (package under R/friends.test) and the
# Bioconductor copy (package at the repository root).  This script transfers
# per-file commit chains between them.
#
# For every file tracked in both package trees it looks for a starting point:
# the most recent commit in the source history at which the file's content
# equals the file's current content in the target.  Every source commit that
# touched the file after that point is then replayed onto the target, keeping
# the original message, author and author date.  Commit ids necessarily
# differ; the sequence, the diffs and the resulting content do not.
#
# If any shared file has no such starting point, both sides changed it
# independently.  Nothing is replayed at all in that case -- the script only
# reports what it found.
#
# Usage: _sync_engine.sh SRC_REPO SRC_PREFIX DST_REPO DST_PREFIX [--dry-run]
#
# A prefix of "." means the package sits at the repository root.  Only the
# currently checked out branch of each repository takes part.

set -euo pipefail

if [ "$#" -lt 4 ]; then
    echo "usage: $(basename "$0") SRC_REPO SRC_PREFIX DST_REPO DST_PREFIX [--dry-run]" >&2
    exit 2
fi

SRC_REPO=$1
SRC_PREFIX=$2
DST_REPO=$3
DST_PREFIX=$4
shift 4

DRY_RUN=0
if [ "${1:-}" = "--dry-run" ]; then
    DRY_RUN=1
elif [ -n "${1:-}" ]; then
    echo "unknown argument: $1" >&2
    exit 2
fi

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

# --------------------------------------------------------------------------
# helpers
# --------------------------------------------------------------------------

# Join a package prefix and a package-relative path.
join_path() {
    if [ "$1" = "." ] || [ -z "$1" ]; then
        printf '%s' "$2"
    else
        printf '%s/%s' "$1" "$2"
    fi
}

# List files tracked under a package prefix, as package-relative paths.
list_files() {
    local repo=$1 prefix=$2 line
    if [ "$prefix" = "." ] || [ -z "$prefix" ]; then
        git -C "$repo" ls-files
    else
        git -C "$repo" ls-files -- "$prefix" | while IFS= read -r line; do
            printf '%s\n' "${line#"$prefix"/}"
        done
    fi
}

# Blob id of a path at a commit, empty if the path does not exist there.
# Blob ids are content addresses, so they are comparable across repositories.
blob_at() {
    git -C "$1" rev-parse --verify --quiet "$2:$3" 2>/dev/null || true
}

# Commits touching a path, newest first.
commits_for() {
    git -C "$1" rev-list HEAD -- "$2"
}

require_clean() {
    local repo=$1 label=$2
    if ! git -C "$repo" rev-parse --git-dir >/dev/null 2>&1; then
        echo "error: $label ($repo) is not a git repository" >&2
        exit 2
    fi
    if [ -n "$(git -C "$repo" status --porcelain --untracked-files=no)" ]; then
        echo "error: $label ($repo) has uncommitted changes to tracked files;" >&2
        echo "       commit or stash them first (untracked files are fine)" >&2
        exit 2
    fi
}

require_clean "$SRC_REPO" "source"
require_clean "$DST_REPO" "target"

SRC_BRANCH=$(git -C "$SRC_REPO" branch --show-current)
DST_BRANCH=$(git -C "$DST_REPO" branch --show-current)
: "${SRC_BRANCH:=<detached HEAD>}"
: "${DST_BRANCH:=<detached HEAD>}"

echo "source: $SRC_REPO [$SRC_BRANCH] prefix=$SRC_PREFIX"
echo "target: $DST_REPO [$DST_BRANCH] prefix=$DST_PREFIX"
echo

# --------------------------------------------------------------------------
# which files take part
# --------------------------------------------------------------------------

list_files "$SRC_REPO" "$SRC_PREFIX" | sort > "$WORK/src_files"
list_files "$DST_REPO" "$DST_PREFIX" | sort > "$WORK/dst_files"
comm -12 "$WORK/src_files" "$WORK/dst_files" > "$WORK/shared"

if [ ! -s "$WORK/shared" ]; then
    echo "error: the two trees share no tracked file; check the prefixes" >&2
    exit 2
fi

# --------------------------------------------------------------------------
# analysis: find the starting point for every shared file
# --------------------------------------------------------------------------

: > "$WORK/plan"      # <commit>\t<relative path>, one line per file per commit
: > "$WORK/replay"    # files with something to replay
: > "$WORK/diverged"  # files that cannot be replayed
in_sync=0

while IFS= read -r rel; do
    src_path=$(join_path "$SRC_PREFIX" "$rel")
    dst_path=$(join_path "$DST_PREFIX" "$rel")

    src_head_blob=$(blob_at "$SRC_REPO" HEAD "$src_path")
    dst_head_blob=$(blob_at "$DST_REPO" HEAD "$dst_path")

    if [ "$src_head_blob" = "$dst_head_blob" ]; then
        in_sync=$((in_sync + 1))
        continue
    fi

    # Walk the source history of this file from newest to oldest and stop at
    # the first commit whose content matches what the target has now.  Taking
    # the most recent match keeps the replayed chain as short as possible.
    chain=""
    base=""
    while IFS= read -r commit; do
        if [ "$(blob_at "$SRC_REPO" "$commit" "$src_path")" = "$dst_head_blob" ]; then
            base=$commit
            break
        fi
        chain="$commit${chain:+ }$chain"   # prepend: ends up oldest first
    done < <(commits_for "$SRC_REPO" "$src_path")

    if [ -z "$base" ]; then
        # No starting point.  Say whether the opposite direction would work,
        # which is the common case when the target is simply ahead.
        hint="both sides changed it independently"
        while IFS= read -r commit; do
            if [ "$(blob_at "$DST_REPO" "$commit" "$dst_path")" = "$src_head_blob" ]; then
                hint="target is ahead; run the opposite script"
                break
            fi
        done < <(commits_for "$DST_REPO" "$dst_path")
        printf '%s\t%s\n' "$rel" "$hint" >> "$WORK/diverged"
        continue
    fi

    n=0
    for commit in $chain; do
        printf '%s\t%s\n' "$commit" "$rel" >> "$WORK/plan"
        n=$((n + 1))
    done
    printf '%s\t%s\t%s\n' "$rel" "$n" "$base" >> "$WORK/replay"
done < "$WORK/shared"

# --------------------------------------------------------------------------
# report
# --------------------------------------------------------------------------

shared_n=$(wc -l < "$WORK/shared" | tr -d ' ')
replay_n=$(wc -l < "$WORK/replay" | tr -d ' ')
diverged_n=$(wc -l < "$WORK/diverged" | tr -d ' ')

echo "shared files : $shared_n"
echo "already equal: $in_sync"
echo "to replay    : $replay_n"
echo "diverged     : $diverged_n"
echo

if [ "$diverged_n" -gt 0 ]; then
    echo "no starting point for:"
    while IFS=$'\t' read -r rel hint; do
        printf '  %-44s %s\n' "$rel" "$hint"
    done < "$WORK/diverged"
    echo
    echo "nothing was replayed; resolve these files by hand first."
    exit 1
fi

if [ "$replay_n" -eq 0 ]; then
    echo "the two trees are already in sync; nothing to do."
    exit 0
fi

# Order the commits to replay the way the source history orders them.
cut -f1 "$WORK/plan" | sort -u > "$WORK/plan_commits"
git -C "$SRC_REPO" rev-list --reverse --topo-order HEAD \
    | grep -Fx -f "$WORK/plan_commits" > "$WORK/ordered"

commit_n=$(wc -l < "$WORK/ordered" | tr -d ' ')

echo "replaying $commit_n commit(s):"
while IFS= read -r commit; do
    subject=$(git -C "$SRC_REPO" log -1 --format=%s "$commit")
    files=$(awk -F'\t' -v c="$commit" '$1 == c { print $2 }' "$WORK/plan" | tr '\n' ' ')
    printf '  %s  %s\n' "$(git -C "$SRC_REPO" rev-parse --short "$commit")" "$subject"
    printf '      %s\n' "$files"
done < "$WORK/ordered"
echo

if [ "$DRY_RUN" -eq 1 ]; then
    echo "--dry-run: stopping before touching $DST_REPO"
    exit 0
fi

# --------------------------------------------------------------------------
# replay
# --------------------------------------------------------------------------

made=0
while IFS= read -r commit; do
    while IFS= read -r rel; do
        src_path=$(join_path "$SRC_PREFIX" "$rel")
        dst_path=$(join_path "$DST_PREFIX" "$rel")
        if git -C "$SRC_REPO" cat-file -e "$commit:$src_path" 2>/dev/null; then
            mkdir -p "$DST_REPO/$(dirname "$dst_path")"
            git -C "$SRC_REPO" cat-file blob "$commit:$src_path" \
                > "$DST_REPO/$dst_path"
            git -C "$DST_REPO" add -- "$dst_path"
        else
            git -C "$DST_REPO" rm -q -f --ignore-unmatch -- "$dst_path"
        fi
    done < <(awk -F'\t' -v c="$commit" '$1 == c { print $2 }' "$WORK/plan")

    if git -C "$DST_REPO" diff --cached --quiet; then
        # The chain reached this commit but the selected files did not change
        # in it -- nothing to record.
        continue
    fi

    git -C "$SRC_REPO" log -1 --format=%B "$commit" > "$WORK/msg"
    GIT_AUTHOR_NAME=$(git -C "$SRC_REPO" log -1 --format=%an "$commit") \
    GIT_AUTHOR_EMAIL=$(git -C "$SRC_REPO" log -1 --format=%ae "$commit") \
    GIT_AUTHOR_DATE=$(git -C "$SRC_REPO" log -1 --format=%aI "$commit") \
        git -C "$DST_REPO" commit --quiet --no-verify -F "$WORK/msg"
    made=$((made + 1))
done < "$WORK/ordered"

echo "done: $made commit(s) written to $DST_REPO [$DST_BRANCH]"
echo "nothing was pushed."
