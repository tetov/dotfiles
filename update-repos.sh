#!/usr/bin/env sh

if [ ! -d $HOME/.vim/bundle/Vundle.vim ] ; then
    mkdir -p ~/.vim/bundle
    git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
fi

git -C "$HOME/tetov-dotfiles" submodule init
git -C "$HOME/tetov-dotfiles" submodule update --remote
git -C "$HOME/.vim/bundle/Vundle.vim" pull

# Disable fsck check of git repos to get around issue described here:
# https://github.com/mileszs/ack.vim/issues/204
git config --global fetch.fsckObjects false

vim +PluginUpdate +qall

git config --global fetch.fsckObjects true
