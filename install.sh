#!/bin/bash
set -e
# Install Homebrew (if not already installed)
if ! command -v brew &>/dev/null; then
    # Download and run the official Homebrew installer
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    # Add brew to PATH for the current session (Apple Silicon path)
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi
# Install chezmoi (dotfile manager)
brew install chezmoi
# Apply dotfiles, run install scripts
chezmoi apply
