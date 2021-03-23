#!/usr/bin/env zsh

stow_prompt() {
    readonly dir_name=${1:?"Directory name must be specified"}

    # http://zsh.sourceforge.net/Doc/Release/Shell-Builtin-Commands.html#index-read
    if read -q "choice?Stow $dir_name (y/N)"; then
        stow --no-folding "$dir_name"
        echo "\nStowed $dir_name"
    else
        echo "\nSkipped $dir_name"
    fi
}

stow_prompt "cli"
stow_prompt "gui"
stow_prompt "rtorrent"
stow_prompt "vim"
stow_prompt "virtualenvwrapper"
