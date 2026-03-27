#!/bin/bash
set -e
xcode-select -p &>/dev/null || xcode-select --install
if [ ! -d "$HOME/.emacs.d" ]; then
    git clone --depth=1 https://github.com/doomemacs/doomemacs "$HOME/.emacs.d"
    "$HOME/.emacs.d/bin/doom" install
fi
