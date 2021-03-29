# Install `zinit` if not installed
if [ ! -d "${HOME}/.zinit" ]; then
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma/zinit/master/doc/install.sh)"
fi
#
## Added by Zinit's installer
source ~/.zinit/bin/zinit.zsh
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

### End of Zinit installer's chunk

# Install `fzf` binary and tmux helper script
zinit ice wait lucid from"gh-r" as"command"
zinit load junegunn/fzf-bin

zinit ice wait lucid as"command" pick"bin/fzf-tmux"
zinit load junegunn/fzf

# Create and bind multiple widgets using fzf
zinit ice wait lucid multisrc"shell/{completion,key-bindings}.zsh" \
    id-as"junegunn/fzf_completions" pick"/dev/null"
zinit load junegunn/fzf

export FZF_DEFAULT_COMMAND='fd --type f --color=always || rg --files --hidden || find .'
# Use ctrl+o to open selected file(s) in vim
export FZF_DEFAULT_OPTS="--bind='ctrl-o:execute(vim {})+abort'"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
# # Using bat as previewer
export FZF_CTRL_T_OPTS="--preview-window 'right:60%' --preview 'bat --color=always --style=header,grid --line-range :300 {}'"
# Changing from ** to ~~ the trigger for autocompletion in shell
export FZF_COMPLETION_TRIGGER='~~'

_fzf_compgen_path() {
  fd --hidden --follow --exclude ".git" . "$"
}

# Use fd to generate the list for directory completion
_fzf_compgen_dir() {
  fd --type d --hidden --follow --exclude ".git" . "$1"
}

    # edit selected
fe() {
    IFS=$'\n' files=($(fzf-tmux --query="$1" --multi --select-1 --exit-0))
    [[ -n "$files" ]] && ${EDITOR:-vim} "${files[@]}"
}

# cd into selected
fd() {
    DIR=`find * -maxdepth 0 -type d -print 2> /dev/null | fzf-tmux` \
        && cd "$DIR"
}

bindkey '^R' fzf-history-widget

##### BEGIN Zinit stuff #####
### needs: zinit, fzf
zinit ice atclone"dircolors -b LS_COLORS > clrs.zsh" \
    atpull'%atclone' pick"clrs.zsh" nocompile'!' \
    atload'zstyle ":completion:*" list-colors “${(s.:.)LS_COLORS}”'
zinit light trapd00r/LS_COLORS

# Load the pure theme, with zsh-async library that's bundled with it.
zinit ice pick"async.zsh" src"pure.zsh"
zinit light sindresorhus/pure

#Pure
autoload -U compinit promptinit
export PURE_CMD_MAX_EXEC_TIME=60
# Make pure theme single line
prompt_newline='%666v'
PROMPT=" $PROMPT"
zstyle :prompt:pure:user color green
zstyle :prompt:pure:host color green
zstyle :prompt:pure:git:branch color white
zstyle :prompt:pure:git:stash show yes

# z
zinit ice wait blockf lucid
zinit light rupa/z

# z tab completion
zinit ice wait lucid
zinit light changyuheng/fz

# z / fzf (ctrl-g)
zinit ice wait lucid
zinit light andrewferrier/fzf-z

# cd
zinit ice wait lucid
zinit light changyuheng/zsh-interactive-cd

# History search by `Ctrl+R`
zinit ice wait lucid
zinit load zdharma/history-search-multi-word

# Tab completions
zinit ice wait lucid blockf atpull'zinit creinstall -q .'
zinit light zsh-users/zsh-completions

# Syntax highlighting
zinit ice wait lucid atinit'zpcompinit; zpcdreplay'
zinit light zdharma/fast-syntax-highlighting

##### END Zinit stuff #####
