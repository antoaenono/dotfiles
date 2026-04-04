#!/bin/sh
# Stash tracked files and remove untracked originals.
# Usage: stash.sh <tmpdir> <tracked file> [<tracked file> ...]
set -e

TMPDIR="$1"
shift
TRACKED="$@"

# Stash only the tracked files
git stash push -- $TRACKED

# Verify stash was created
if ! git stash list | head -1 | grep -q "stash@{0}"; then
  echo "ERROR: stash was not created" >&2
  exit 1
fi

# Remove untracked originals (safe - already in tmpdir)
UNTRACKED=$(git ls-files --others --exclude-standard)
for f in $UNTRACKED; do
  rm "$f"
done
