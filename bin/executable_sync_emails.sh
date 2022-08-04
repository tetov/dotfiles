#!/usr/bin/sh

MBSYNC_LOCKFILE="$XDG_CACHE_HOME/sync_emails_lock"

MBSYNCRC="$XDG_CONFIG_HOME/mbsyncrc"
MU_LOCKFILE="$XDG_CACHE_HOME/mu/xapian/flintlock"

if [ $# -ne 0 ] ; then  # if positional args
    GROUPS_OR_CHANNELS="$*" # all positional args
else
    GROUPS_OR_CHANNELS="--all"
fi

# from docstore.mik.ua/orelly/unix3/upt/ch36_27.htm
until (umask 222; echo $$ >$MBSYNC_LOCKFILE) 2>/dev/null # test & set

do
 # Optional message - show lockfile owner and creation time:
 set x $(ls -l $MBSYNC_LOCKFILE)
 echo "Waiting for user $4 (working since $7 $8 $9)..."
 sleep 5
done

echo "Aquired lock"

# shellcheck disable=SC2086
mbsync --config "$MBSYNCRC" --verbose $GROUPS_OR_CHANNELS

# Run mu index, if that doesn't work (since mu4e has locked the database) run
# mu4e-update-index
if [ ! -e "$MU_LOCKFILE" ] ; then
    echo "Updating using mu"
    mu index
else
    echo "Updating using mu4e"
    emacsclient -e '(mu4e-update-index)'
fi

rm -f "$MBSYNC_LOCKFILE" # unlock
