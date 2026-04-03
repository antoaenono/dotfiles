#!/bin/sh
grep -oE ':tangle "([^"]+)"' ~/.local/share/chezmoi/config.org \
  | sed 's/:tangle "//;s/"//' \
  | xargs -I{} dirname ~/.local/share/chezmoi/{} \
  | sort -u \
  | xargs mkdir -p
