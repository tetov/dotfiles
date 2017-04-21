#!/usr/bin/env bash

command -v stow >/dev/null 2>&1 || { echo "Stow is required. Please install before running again." >&2; exit 1; }

cd ~/tetov-dotfiles || { echo "Can't cd to ~/tetov-dotfiles." >&2; exit 1; }

stow bash
stow zsh
stow vim
stow lftp
stow git

if [[ $OSTYPE == darwin* ]] ; then
  stow karabiner-elements
  stow hammerspoon
elif [ "$OSTYPE" = "linux-gnu" ]; then
	stow rmate
else
	echo "Unknown OS type"
fi

exit 0
