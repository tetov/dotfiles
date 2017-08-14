# Source general shell env
. $HOME/.shellrc

setopt HIST_IGNORE_DUPS

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

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

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
