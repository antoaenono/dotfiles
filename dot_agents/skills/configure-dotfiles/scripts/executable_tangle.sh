#!/bin/sh
set -eu

elisp_file="${XDG_CONFIG_HOME:-$HOME/.config}/doom/lisp/tangle-moi.el"

if [ ! -r "$elisp_file" ]; then
  echo "Tangle failed: shared Emacs library not found at $elisp_file." >&2
  exit 1
fi

if command -v emacsclient >/dev/null 2>&1 \
    && emacsclient --eval t >/dev/null 2>&1; then
  escaped_elisp_file=$(printf '%s' "$elisp_file" \
    | sed 's/\\/\\\\/g; s/"/\\"/g')
  exec emacsclient --eval \
    "(progn (load \"$escaped_elisp_file\" nil t) (tangle-moi-tangle))"
fi

if ! command -v emacs >/dev/null 2>&1; then
  echo "Tangle failed: Emacs is not installed or not on PATH." >&2
  exit 127
fi

exec emacs --batch \
  --load "$elisp_file" \
  --funcall tangle-moi-tangle-batch
