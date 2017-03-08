#!/usr/bin/env bash

git -C $dotfiles_dir submodule update
antigen update
git -C $HOME/.vim/bundle/Vundle.vim pull
vim +PluginUpdate
