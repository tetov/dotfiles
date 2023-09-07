[[ -v INSIDE_EMACS ]] && return 0 # don't use fzf in vterm

export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_COMPLETION_TRIGGER='~~'
# export FZF_DEFAULT_OPTS='-m --height 50% --border'

[[ -r /usr/share/fzf/shell/key-bindings.zsh ]] && . /usr/share/fzf/shell/key-bindings.zsh
[[ -r /usr/share/zsh/site-functions/fzf ]] && . /usr/share/zsh/site-functions/fzf
