# Source general shell env
. $HOME/.shellrc

# history settings
HISTFILE="$HOME/.zsh_history"
HISTSIZE=50000
SAVEHIST=10000

setopt extended_history       # record timestamp of command in HISTFILE
setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_ignore_dups       # ignore duplicated commands history list
setopt hist_ignore_space      # ignore commands that start with space
setopt hist_verify            # show command with history expansion to user before running it
setopt inc_append_history     # add commands to HISTFILE in order of execution
setopt share_history          # share command history dat

setopt auto_cd # cd by typing directory name if it's not a command
setopt correct_all # autocorrect commands
setopt complete_in_word # complete where cursor is
setopt always_to_end # always put cursor at end after completing

unsetopt BG_NICE # not nice since WSL is not nice

source <(antibody init)
antibody bundle < ~/.zsh_plugins.txt

#Pure
autoload -U compinit promptinit
PURE_CMD_MAX_EXEC_TIME=60
# Make pure theme single line
prompt_newline='%666v'
PROMPT=" $PROMPT"

# forces zsh to realize new commands
zstyle ':completion:*' completer _oldlist _expand _complete _match _ignored _approximate

# matches case insensitive and partial and substring
zstyle ':completion:*' matcher-list 'r:|=*' 'l:|=* r:|=*'


# pasting with tabs doesn't perform completion
zstyle ':completion:*' insert-tab pending

# rehash if command not found (possibly recently installed)
zstyle ':completion:*' rehash true


# Group matches and describe.
zstyle ':completion:*:*:*:*:*' menu select

# menu if nb items > 2
zstyle ':completion:*' menu select=2

zstyle ':completion:*:default' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' list-colors  'reply=( "=(#b)(*$PREFIX)(?)*=00=$color[green]=$color[bg-green]" )'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'

set -o vi # Make fzf work with vi mode in zsh

# fzf installed from git
[ -e ~/.fzf.zsh ] && source ~/.fzf.zsh

# fzf on arch
if [ -e /usr/share/fzf/key-bindings.zsh ] ; then
    source /usr/share/fzf/key-bindings.zsh
    source /usr/share/fzf/completion.zsh
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

# cache completions https://blog.callstack.io/supercharge-your-terminal-with-zsh-8b369d689770
autoload -Uz compinit
typeset -i updated_at=$(date +'%j' -r ~/.zcompdump 2>/dev/null || stat -f '%Sm' -t '%j' ~/.zcompdump 2>/dev/null)
if [ $(date +'%j') != $updated_at ]; then
  compinit -i
else
  compinit -C -i
fi
