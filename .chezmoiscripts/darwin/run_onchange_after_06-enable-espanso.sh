#!/bin/bash
set -e
espanso_bin="/Applications/Espanso.app/Contents/MacOS/espanso"

# Espanso manages its own macOS LaunchAgent for starting at login.
if ! "$espanso_bin" service check >/dev/null 2>&1; then
    "$espanso_bin" service register
fi

if ! "$espanso_bin" status >/dev/null 2>&1; then
    "$espanso_bin" start
fi
