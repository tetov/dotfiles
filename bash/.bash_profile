# shellcheck shell=sh
export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad

export VISUAL=vim
export EDITOR="$VISUAL"

# ALIASES
alias ls='ls -FG'
alias ll='ls -lh'
alias la='ls -A'
alias l='ls'
alias ..='cd ..'

if [ -f "$(brew --prefix)"/etc/bash_completion ]; then
    . "$(brew --prefix)"/etc/bash_completion
fi
