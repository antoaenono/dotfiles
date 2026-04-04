#!/bin/sh
cd ~/.local/share/chezmoi && emacs --batch \
  --eval "(require 'org)" \
  --eval "(org-babel-tangle-file \"config.org\")"
