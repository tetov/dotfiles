#!/usr/bin/env sh

if [ ! -d $HOME/.vim/bundle/Vundle.vim ] ; then
    mkdir -p ~/.vim/bundle
    git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
fi

git -C "$HOME/tetov-dotfiles" submodule init
git -C "$HOME/tetov-dotfiles" submodule update --remote
git -C "$HOME/.vim/bundle/Vundle.vim" pull

vim +PluginUpdate +qall
