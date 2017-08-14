export VISUAL=vim
export EDITOR="$VISUAL"

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

setopt HIST_IGNORE_DUPS

dotfiles_dir=$HOME/tetov-dotfiles

path+=$HOME/bin
path+=$dotfiles_dir/bin

LS_COLORS='no=00;37:fi=00:di=00;33:ln=04;36:pi=40;33:so=01;35:bd=40;33;01:'
export LS_COLORS
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Load variables containing server adresses and user names
. $dotfiles_dir/bin/source_confidentials

ZSHA_BASE=$dotfiles_dir/antigen
. $ZSHA_BASE/antigen.zsh

antigen use oh-my-zsh

antigen bundle command-not-found
antigen bundle zsh-users/zsh-completions
antigen bundle djui/alias-tips
antigen bundle git

antigen theme gentoo

# zsh-syntax-highlighting needs to go last
antigen bundle zsh-users/zsh-syntax-highlighting

antigen apply

# Aliases
. $dotfiles_dir/aliases

if [[ $OSTYPE == darwin* ]] ; then
    export HOMEBREW_NO_ANALYTICS=1

	path+=/usr/local/sbin
    path+="$HOME/.rbenv/bin:$PATH"

    eval "$(rbenv init -)"
fi

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
