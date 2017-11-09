#!/usr/bin/env sh

_promptUser () {
    local choice

    printf "$1 [y/n]: "
    read choice

    if echo "$choice" | grep -iq "^y" ; then
        return 0
    else
        return 1
    fi
}

_checkDep () {
    command -v "$1" >/dev/null 2>&1 \
        || { echo "$1" \
        "is not installed. Please install before running again." ; \
        return 1; }
}

_selectPKGRepo () {
    if command -v brew >/dev/null 2>&1 ; then
        pkgCommand="brew"
    elif command -v apt-get >/dev/null 2>&1 ; then
        pkgCommand="apt-get"
    elif command -v pacman >/dev/null 2>&1 ; then
        pkgCommand="pacman"
    else
        return 1
    fi

    export pkgCommand
}
