#!/bin/sh

# install gnome-shell-extensions-sync, which installs extensions itself
~/bin/gnome-shell-extension-installer 1486
gnome-extensions enable extensions-sync@elhan.io

echo 'You may need to reload GNOME Shell to recognise new extension by hitting Alt + F2 and entering "r"'
