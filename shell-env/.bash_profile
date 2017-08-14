export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad

export VISUAL=vim
export EDITOR="$VISUAL"

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export PS1='\u@\h \w \$ '

dotfiles_dir=$HOME/tetov-dotfiles

path+=$HOME/bin
path+=$dotfiles_dir/bin

LS_COLORS='no=00;37:fi=00:di=00;33:ln=04;36:pi=40;33:so=01;35:bd=40;33;01:'
export LS_COLORS

# Load variables containing server adresses and user names
. "$dotfiles_dir/bin/source_confidentials"

# Source aliases from dot-di# Source aliases from dot-dir
. "$dotfiles_dir/aliases"

if [ -f "$(brew --prefix)"/etc/bash_completion >/dev/null 2>&1 ]; then
    . "$(brew --prefix)"/etc/bash_completion
fi
