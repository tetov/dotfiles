#!/bin/bash
PAGEANT=$HOME/.ssh/wsl2-ssh-pageant.exe
ss -a | grep -q "$SSH_AUTH_SOCK"
if [ $? -ne 0 ]; then
    rm -f "$SSH_AUTH_SOCK"
    (setsid nohup socat UNIX-LISTEN:"$SSH_AUTH_SOCK",fork EXEC:"$PAGEANT" &)
    SSH_PID=$!
fi
wait $SSH_PID
ss -a | grep -q "$GPG_AGENT_SOCK"
if [ $? -ne 0 ]; then
    rm -rf $GPG_AGENT_SOCK
    (setsid nohup socat UNIX-LISTEN:$GPG_AGENT_SOCK,fork EXEC:"$PAGEANT --gpg S.gpg-agent" &)
    GPG_PID=$!
    wait $GPG_PID
fi

