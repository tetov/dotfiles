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

set -o vi # Make fzf work with vi mode in zsh

# fzf installed from git
[ -e ~/.fzf.zsh ] && source ~/.fzf.zsh

# fzf on arch
if [ -e /usr/share/fzf/key-bindings.zsh ] ; then
    source /usr/share/fzf/key-bindings.zsh
    source /usr/share/fzf/completion.zsh
fi

# fzf + ag configuration
if command -v fzf >/dev/null && command -v rg >/dev/null ; then
    export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'
    export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
    export FZF_ALT_C_COMMAND="$FZF_DEFAULT_COMMAND"
    export FZF_DEFAULT_OPTS='
    --color fg:242,bg:236,hl:65,fg+:15,bg+:239,hl+:108
    --color info:108,prompt:109,spinner:108,pointer:168,marker:168
    '
fi

# Vi mode in zsh, taken from https://dougblack.io/words/zsh-vi-mode.html
bindkey -v

bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^R' fzf-history-widget

export KEYTIMEOUT=1
