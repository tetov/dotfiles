#!/usr/bin/env sh

"$HOME/bin/git-sync" "$(chezmoi source-path)"
systemctl --user start git-sync.service
