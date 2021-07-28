#!/usr/bin/env zsh

setopt extended_glob  # needed for glob after --stow

STOW_DIR="$HOME/dotfiles"  # Should be repo dir
TARGET_DIR=$HOME

stow --dir="$STOW_DIR" \
  --target="$TARGET_DIR" \
  --no-folding \
  --ignore=deps \
  --ignore=bin \
  --verbose \
  --stow ^(deps|bin)*/  # matches dirs but not those with names containing deps & bin

