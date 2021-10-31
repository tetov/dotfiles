# Clone zcomet if necessary
if [[ ! -f ~/.zcomet/bin/zcomet.zsh ]]; then
  command git clone https://github.com/agkozak/zcomet.git ~/.zcomet/bin
fi

source ~/.zcomet/bin/zcomet.zsh

zcomet load sindresorhus/pure async.zsh pure.zsh
PURE_CMD_MAX_EXEC_TIME=60
# Make pure theme single line
prompt_newline='%666v'
PROMPT=" $PROMPT"

# zstyle :prompt:pure:user color green
# zstyle :prompt:pure:host color green
# zstyle :prompt:pure:git:branch color white
zstyle :prompt:pure:git:stash show yes

zcomet load agkozak/zsh-z

zcomet load ohmyzsh plugins/gitfast

zcomet load zdharma-continuum/history-search-multi-word

zcomet load zsh-users/zsh-completions

# update tmux title
zcomet snippet https://raw.githubusercontent.com/jreese/zsh-titles/master/titles.plugin.zsh

zcomet load zdharma-continuum/fast-syntax-highlighting

zcomet load jeffreytse/zsh-vi-mode


