#!/bin/sh

dconf write /org/gnome/shell/extensions/extensions-sync/provider "'Local'"
dconf write /org/gnome/shell/extensions/extensions-sync/backup-file-location \
    "'file://$(chezmoi source-path)/gnome_extension_sync.json'"

busctl --user call org.gnome.Shell /io/elhan/ExtensionsSync io.elhan.ExtensionsSync save