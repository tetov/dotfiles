#!/usr/bin/sh

MBSYNCRC="$XDG_CONFIG_HOME/mbsyncrc"
MU_LOCKFILE="$XDG_CACHE_HOME/mu/xapian/flintlock"

if [ $# -ne 0 ] ; then  # if positional args
    GROUPS_OR_CHANNELS="$*" # all positional args
else
    GROUPS_OR_CHANNELS="--all"
fi

# shellcheck disable=SC2086
mbsync --config "$MBSYNCRC" --verbose $GROUPS_OR_CHANNELS

# Run mu index, if that doesn't work (since mu4e has locked the database) run
# mu4e-update-index
if [ ! -e "$MU_LOCKFILE" ] ; then
    echo "Updating using mu"
    mu index
elif pgrep -x emacs > /dev/null ; then
    echo "Updating using mu4e"
    emacsclient -e '(mu4e-update-index)'
else
    echo "Lockfile exists but emacs is not running. Stale lockfile?"
    exit 1
fi
