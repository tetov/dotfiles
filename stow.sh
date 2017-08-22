#!/usr/bin/env sh

command -v stow >/dev/null 2>&1 || { echo "Stow is required. Please install before running again." >&2; exit 1; }

cd ~/tetov-dotfiles || { echo "Can't cd to ~/tetov-dotfiles." >&2; exit 1; }

stow shell-env
stow vim
stow git
stow lftp
stow ssh

if [ $(uname) = "Darwin" ]; then
  stow karabiner-elements
  stow hammerspoon
fi
