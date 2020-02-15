#!/usr/bin/env sh

command -v stow >/dev/null 2>&1 || { echo "Stow is required. Please install before running again." >&2; exit 1; }

cd "$HOME/tetov-dotfiles" || { echo "Can't cd to ~/tetov-dotfiles." >&2; exit 1; }

mkdir -p "$HOME/.vim/colors" "$HOME/.lftp"

stow cli
stow config
stow vim
stow virtualenvwrapper

