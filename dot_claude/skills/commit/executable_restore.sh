#!/bin/sh
# Restore working tree from stash. Use when something goes wrong
# and no commits have been made yet.
set -e
git stash pop
echo "Restored from stash."
