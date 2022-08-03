#!/usr/bin/sh

MBSYNCRC="$XDG_CONFIG_HOME/mbsyncrc"
LOCK_FILE="$XDG_CACHE_HOME/mu/xapian/flintlock"

CHANNELS="lth" # channel names separated with spaces

mbsync --config "$MBSYNCRC" --verbose --full "$CHANNELS"

# Run mu index, if that doesn't work (since mu4e has locked the database) run
# mu4e-update-index
if [ ! -e "$LOCK_FILE" ] ; then
    echo "Updating using mu"
    mu index
else
    echo "Updating using mu4e"
    emacsclient -e '(mu4e-update-index)'
fi
