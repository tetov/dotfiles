export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad

# ALIASES
alias ls='ls -FG'
alias ll='ls -lh'
alias la='ls -A'
alias l='ls'
alias ..='cd ..'
alias arka='ssh farligm@a.tetov.se'

if [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi
