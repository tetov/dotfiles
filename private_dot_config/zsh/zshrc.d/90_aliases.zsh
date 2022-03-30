if (( $+commands[nvim] )) ; then
    alias v="nvim"
else
    alias v='vim'
fi

if (( $+commands[exa] )) ; then
    alias ls="exa --group-directories-first"
    alias ll="ls -l --group --git"
else
    alias ls='ls -F --color=auto'
    alias ll='ls -lh'
fi

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

alias cm='chezmoi'

cmcd() {
    cd $(chezmoi source-path)
}

command -v makechrootpkg >/dev/null \
    && [[ -v CHROOT ]] \
    && alias mcpkg='makechrootpkg -cu -r $CHROOT -l $(basename $(pwd)) -- PACKAGER=$PACKAGER'

if (( $+commands[gpg] )) ; then
    alias clean_gpg_socks="rm $XDG_RUNTIME_DIR/gnupg/S.*"
    alias restart_gpg="gpg-connect-agent reloadagent /bye"
fi
