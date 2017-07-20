#!/usr/bin/env sh

git -C "$HOME/tetov-dotfiles" submodule update
git -C "$HOME/.vim/bundle/Vundle.vim" pull
vim +PluginUpdate +qall
