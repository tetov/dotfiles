export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_COMPLETION_TRIGGER='~~'
# export FZF_DEFAULT_OPTS='-m --height 50% --border'

function load_additional_zsh_files() {
    _source_if_exists /usr/share/fzf/shell/key-bindings.zsh
    _source_if_exists /usr/share/zsh/site-functions/fzf
}

# functions that runs after zsh-vi-mode...
zvm_after_init_commands+=(load_additional_zsh_files)
