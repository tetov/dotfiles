#!/bin/sh

# mkdir $HOME/gdrive
systemctl --user enable rclone@gdrive.service
systemctl --user start rclone@gdrive.service

systemctl --user enable pull-org-repo.timer
systemctl --user start pull-org-repo.timer
