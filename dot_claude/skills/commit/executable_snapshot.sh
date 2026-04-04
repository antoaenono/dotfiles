#!/bin/sh
# Snapshot all uncommitted changes to a temp directory.
# Prints the temp dir path on stdout.
# Usage: snapshot.sh
set -e

TMPDIR=$(mktemp -d /tmp/commit-snapshot-XXXXXX)

# Get modified tracked files and new untracked files
TRACKED=$(git diff --name-only)
UNTRACKED=$(git ls-files --others --exclude-standard)

for f in $TRACKED $UNTRACKED; do
  mkdir -p "$TMPDIR/$(dirname "$f")"
  cp "$f" "$TMPDIR/$f"
done

# Verify checksums
for f in $TRACKED $UNTRACKED; do
  ORIG=$(shasum "$f" | awk '{print $1}')
  COPY=$(shasum "$TMPDIR/$f" | awk '{print $1}')
  if [ "$ORIG" != "$COPY" ]; then
    echo "ERROR: checksum mismatch for $f" >&2
    exit 1
  fi
done

echo "$TMPDIR"
