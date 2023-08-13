#!/usr/bin/env sh

set -x
set -e

systemctl --user start 'git_sync@\x2elocal-share-chezmoi.service'
systemctl --user start sync_emails@all.service
