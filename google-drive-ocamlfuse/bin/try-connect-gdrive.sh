#!/bin/bash

while true
do
  # check to see if there is a connection by pinging a Google server
    if ping -q -c 1 -W 1 8.8.8.8 >/dev/null; then
        # if connected, mount the drive and break the loop
        google-drive-ocamlfuse $DRIVE_FOLDER
        break
    else
        # if not connected, wait for one second and then check again
        sleep 10
    fi
done
