#!/bin/bash

DL="\"$1\""

SOURCEDIR=farligm@a.tetov.se:/home/farligm/v/rtorrent.data

TARGETDIR=/Volumes/JUDITH/Deaddrop/

rsync -vze 'ssh -q' "$SOURCEDIR"/"$DL" --progress "$TARGETDIR"