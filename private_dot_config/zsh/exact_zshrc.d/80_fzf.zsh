[[ -v INSIDE_EMACS ]] && return 0 # don't use fzf in vterm

export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_COMPLETION_TRIGGER='~~'
# export FZF_DEFAULT_OPTS='-m --height 50% --border'

function load_additional_zsh_files() {
    [[ -r /usr/share/fzf/shell/key-bindings.zsh ]] && . /usr/share/fzf/shell/key-bindings.zsh
    [[ -r /usr/share/zsh/site-functions/fzf ]] && . /usr/share/fzf/shell/key-bindings.zsh
}

# functions that runs after zsh-vi-mode...
zvm_after_init_commands+=(load_additional_zsh_files)
