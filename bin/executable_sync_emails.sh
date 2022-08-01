#!/usr/bin/sh

MBSYNCRC="$XDG_CONFIG_HOME/mbsyncrc"

mbsync --config "$MBSYNCRC" --all --verbose
# Run mu index, if that doesn't work (since mu4e has locked the database) run
# mu4e-update-index
mu index || emacsclient -e '(mu4e-update-index)'
