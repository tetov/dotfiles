#!/usr/bin/env sh
#--------------------------------------------------------------------------
# GPG bridging from WSL gpg to gpg4win gpg-agent.exe
# (needed to use a Yubikey, since WSL cannot access USB devices)

set -eu

usage() {
   echo "Usage: gpgbridge_helper [ --ssh ] [ --wsl2 ] [ -q --quiet ] start|stop|restart"
}

# PATHS
SCRIPT_DIR=/mnt/c/Users/a/.gpgbridge

PIDFILE_WSL=~/.gpgbridge_wsl.pid
LOGFILE_WSL=~/.gpgbridge_wsl.log

_pidfile_win=$SCRIPT_DIR/gpgbridge_win.pid
_logfile_win=$SCRIPT_DIR/gpgbridge_win.log
touch $_pidfile_win $_logfile_win  # Needs to be created otherwise gpgbridge complains
PIDFILE_WIN=$(wslpath -wa $_pidfile_win)
LOGFILE_WIN=$(wslpath -wa $_logfile_win)

SCRIPT_FILE_NAME=gpgbridge.rb

SCRIPT_PATH_WSL=$SCRIPT_DIR/$SCRIPT_FILE_NAME

start_gpgbridge()
{
    if ! command -v ruby.exe >/dev/null 2>&1 ; then
        echo 'No ruby.exe found in path'
        return 1
    fi

    args="$(_quote "$SCRIPT_PATH_WSL") --daemon"

    if [ -n "${PIDFILE_WSL:-}" ] ; then
       args="$args --pidfile $(_quote "$PIDFILE_WSL")"
    fi

    if [ -n "${LOGFILE_WSL:-}" ] ; then
       args="$args --logfile $(_quote "$LOGFILE_WSL")"
    fi

    if [ -n "${PIDFILE_WIN:-}" ] ; then
       args="$args --windows-pidfile $(_quote "$PIDFILE_WIN")"
    fi

    if [ -n "${LOGFILE_WIN:-}" ] ; then
       args="$args --windows-logfile $(_quote "$LOGFILE_WIN")"
    fi

    if [ "$SSH" -eq 1 ] ; then
        args="$args --enable-ssh-support"
        SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
        export SSH_AUTH_SOCK
    fi

    if [ "$WSL2" -eq 1 ] ; then
       wsl_ip="$(ip route | awk '/^default via / {print $3}')"
       args="$args --remote-address $wsl_ip"
    fi

    if [ "$QUIET" -eq 1 ] ; then
        args="$args >/dev/null 2>&1"
    # else
        # args="$args --verbose"
    fi

    rm -f $LOGFILE_WSL

    eval ruby "$args"
}

stop_gpgbridge()
{
   # Kill gpgbridge if running, else return
   pkill -TERM -f 'ruby.*gpgbridge\.rb' || return 0
}

restart_gpgbridge()
{
    stop_gpgbridge
    sleep 1
    start_gpgbridge
}

# From "Rich's sh (POSIX shell) tricks" https://www.etalabs.net/sh_tricks.html
_quote () { printf %s\\n "$1" | sed "s/'/'\\\\''/g;1s/^/'/;\$s/\$/'/" ; }

# Default arguments
SSH=0
WSL2=0
QUIET=0

WSL2=$_gpgbridge_wsl2
SSH=$_gpgbridge_ssh

start_gpgbridge
set +eu
