#!/usr/bin/env bash

# Functions

promptThenRun () {
    local choice

    echo

    read -r -p "$1" choice

    if [[ $choice =~ [yY](es)* ]]; then
        $2
    fi
}

aptInstall () {
    sudo apt-get update

    xargs sudo apt-get -y install <"$HOME"/tetov-dotfiles/Aptfile
}

fzfInstall () {
    git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
    ~/.fzf/install
}

setZshShell () {
    sudo chsh --shell="$(which zsh)" "$USER"
}

cloneDotfilesRepo () {
    git clone https://github.com/tetov/tetov-dotfiles.git ~/tetov-dotfiles

    promptThenRun 'Do you want to run the stow script? ' ~/tetov-dotfiles/stow.sh

    promptThenRun 'Do you want to run the antigen & vundle script? '
    ~/tetov-dotfiles/install-common.sh

}


# INTERACTION

promptThenRun 'Do you want to install fzf ' fzfInstall
promptThenRun 'Set zsh as login shell? ' setZshShell
promptThenRun 'Do you want to install apt packages? ' aptInstall
promptThenRun 'Do you want clone dotfiles repo? ' cloneDotfilesRepo
