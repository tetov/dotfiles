#!/usr/bin/env zsh

PKG=confs  # Dir with files and directories to symlink
STOW_DIR="$HOME/tetov-dotfiles"  # Should be repo dir
TARGET_DIR=$HOME

stow --dir="$STOW_DIR" --target="$TARGET_DIR" --no-folding --stow "$PKG"
