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

# fzf settings
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# fzf via Homebrew
if [ -e /usr/local/opt/fzf/shell/completion.zsh ]; then
  source /usr/local/opt/fzf/shell/key-bindings.zsh
  source /usr/local/opt/fzf/shell/completion.zsh
fi

# fzf + ag configuration
if command -v fzf >/dev/null 2>&1 && command -v ag >/dev/null 2>&1; then
  export FZF_DEFAULT_COMMAND='ag --nocolor -g ""'
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
  export FZF_ALT_C_COMMAND="$FZF_DEFAULT_COMMAND"
  export FZF_DEFAULT_OPTS='
  --color fg:242,bg:236,hl:65,fg+:15,bg+:239,hl+:108
  --color info:108,prompt:109,spinner:108,pointer:168,marker:168
  '
fi
