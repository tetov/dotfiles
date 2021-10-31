zinit pack for ls_colors

# Load the pure theme, with zsh-async library that's bundled with it.
zinit ice pick"async.zsh" src"pure.zsh"
zinit light sindresorhus/pure

#Pure
autoload -U compinit promptinit
export PURE_CMD_MAX_EXEC_TIME=60
# Make pure theme single line
prompt_newline='%666v'
PROMPT=" $PROMPT"
# zstyle :prompt:pure:user color green
# zstyle :prompt:pure:host color green
# zstyle :prompt:pure:git:branch color white
# zstyle :prompt:pure:git:stash show yes

zinit ice wait blockf lucid
zinit light rupa/z

zinit ice wait lucid
zinit load zdharma/history-search-multi-word

zinit ice wait lucid blockf atpull'zinit creinstall -q .'
zinit light zsh-users/zsh-completions

zinit ice wait lucid atinit'zpcompinit; zpcdreplay'
zinit light zdharma/fast-syntax-highlighting

zinit ice depth=1
zinit light jeffreytse/zsh-vi-mode

