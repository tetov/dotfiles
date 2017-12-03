#!/usr/bin/env bash

# Env

source ~/tetov-dotfiles/scripts/support-functions.sh

scriptUser=$(whoami)

# Functions

aptInstall () {
    # TODO: Install AUR pkgs
    test -z $pkgCommand || _selectPKGRepo

    case $pkgCommand in
        "apt-get")
            sudo apt-get update
            xargs sudo apt-get -y install <"$HOME"/tetov-dotfiles/Aptfile
            ;;
        "pacman")
            sudo pacman -Syu
            wget -qO - https://gist.githubusercontent.com/tetov/6dae96b24b30d55af2b132ee5dd54971/raw/ff802031b0bd224212a976257b601f45bdb59487/pacman-list.pkg | sudo pacman -S --needed -
            ;;
        "*")
            echo "Script not configured for " $pkgCommand
            return 1
    esac
}

fzfInstall () {
    test -z $pkgCommand || _selectPKGRepo

    case $pkgCommand in
        "pacman") sudo pacman -Syu fzf ;;
        "brew") brew install fzf ;;
        "apt-get")
            checkDep git || return 1
            git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
            ~/.fzf/install ;;
        *) echo "Something went wrong." ;;
    esac

}

setZshShell () {
    _checkDep zsh || return 1

    # TODO: Test if following works on mac (try contain instead?)
    if [[ $SHELL =~ "zsh" ]]; then
        echo "ZSH is already set as " $scriptUser "'s shell'"
    else
        if [[ $UNAME == "darwin" ]]; then
            sudo chsh --shell="$(which zsh)" "$scriptUser"
        else
            sudo chsh --shell=/bin/zsh "$scriptUser"
        fi
    fi
}

cloneDotfilesRepo () {
    _checkDep git || { echo "Git not installed." ; return 1 ; }

    git clone https://github.com/tetov/tetov-dotfiles.git ~/tetov-dotfiles \
        || { echo "Git clone failed. Exiting." ; return 1 ; }
    }

    # INTERACTION

_promptUser 'Set zsh as login shell?' && setZshShell
_promptUser 'Do you want to install fzf?' && fzfInstall
_promptUser 'Do you want to install apt packages?' && aptInstall
_promptUser 'Do you want to run the stow script?' && ~/tetov-dotfiles/scripts/stow.sh
_promptUser 'Do you want to run the antigen & vundle script?' && ~/tetov-dotfiles/scripts/install-common.sh
