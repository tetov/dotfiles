if (( $+commands[exa] )) ; then
    alias ls="exa --group-directories-first"
else
    alias ls='ls -F --color=auto'
fi

alias ll='ls -lh'
alias la='ls -a'
alias l='ls'

alias mkdir='mkdir -pv'
alias rmi='rm -i'

alias cp='cp -i'
alias mv='mv -i'

alias ..='cd ..'
alias _='sudo '

alias grep='grep -i --color=auto'
#
if (( $+commands[rtcontrol] )) ; then
    alias rthot="watch -n30 'rtcontrol -rs up,down,name xfer=+0 2>&1'"
    alias rtmsg="rtcontrol -s alias,message,name 'message=?*' 'message=!*Tried?all?trackers*'"
    alias rtmsgstats="rtcontrol -q -s alias,message -o alias,message 'message=?*' 'message=!*Tried?all?trackers*' | uniq -c"
    alias rt2days="rtcontrol --column-headers -scompleted -ocompletion completed=-2d"
    alias rtstats="rtcontrol -q -o alias -s alias \\* | uniq -c"
    alias rti="rtcontrol is_complete=no"
    alias rttrump='rtcontrol -s alias,message,name message="*nregistered*"'
fi

# plex
alias notlinked='find . -type f -links 1 \( \! -iname "*.jpg" \! -iname "*.nfo" \! -iname "*.png" \! -iname "*.srt" \)'

(( $+commands[rolldice] )) && alias r='rolldice -s '
(( $+commands[bat] )) && alias cat="bat"

# todo.txt
if [[ -v $TODO_DIR && -e $TODO_DIR/{todo,done}.txt ]] ; then
    alias todo='vim $TODO_DIR/{todo,done}.txt'
fi

if (( $+commands[git] )) ; then
    alias g='git'

    alias ga='git add'

    alias gb='git branch'

    alias gc='git commit -v'
    alias gcd='cd "$(git rev-parse --show-toplevel)"'
    alias gcmsg='git commit -m'
    alias gco='git checkout'

    alias gd='git diff'

    alias gf='git fetch'

    alias gl='git pull'

    alias gp='git push'

    alias gr='git remote'
    alias gra='git remote add'
    alias grb='git rebase'

    alias gst='git status'

fi

command -v makechrootpkg >/dev/null \
    && [[ -v CHROOT ]] \
    && alias mcpkg='makechrootpkg -c -r $CHROOT -l $(basename $(pwd)) -- PACKAGER=$PACKAGER'
