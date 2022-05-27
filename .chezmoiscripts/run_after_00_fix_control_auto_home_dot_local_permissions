#!/bin/sh
# unison backup at automatic control needs 711 on .local but chezmoi resets it
# to 700

if [ $(id -gn) = "regler" -a $(dirname $HOME) = "/home" ] ; then
    chmod 711 "$HOME/.local"
fi
