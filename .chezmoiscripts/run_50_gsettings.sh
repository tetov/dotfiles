#!/usr/bin/env sh

command -v gsettings >/dev/null || exit 0

gsettings set org.gnome.desktop.interface color-scheme "prefer-dark"
