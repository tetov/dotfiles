# Source general shell env
. ~/.shellrc

# Path
typeset -U path
path=(~/bin $DOTFILES/bin ~/.cargo/bin $path)

fpath+=$DOTFILES/zshfuncs

# history settings
HISTFILE=~/.zsh_history
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
zstyle :prompt:pure:user color green
zstyle :prompt:pure:host color green
zstyle :prompt:pure:git:branch color white
zstyle :prompt:pure:git:stash show yes
# forces zsh to realize new commands
zstyle ':completion:*' completer _oldlist _expand _complete _match _ignored _approximate

# matches case insensitive and partial and substring
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'm:{a-zA-Z}={A-Za-z} l:|=* r:|=*'

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

autoload -U open

# virtualenvwrapper
export WORKON_HOME=~/.virtualenvs
export PROJECT_HOME=~/l-repos

VW_PATHS=( /usr/bin/virtualenvwrapper.sh \
           /usr/share/virtualenvwrapper/virtualenvwrapper.sh )

for p in $VW_PATHS ; do
  if [[ -e $p ]] ; then
    . $p
    autoload -U add-zsh-hook auto_workon
    add-zsh-hook -Uz chpwd auto_workon
    auto_workon
    break
  fi
done

set -o vi # Make fzf work with vi mode in zsh

# fzf installed from git
FZF_PATHS=(~ /usr/share/fzf)

for p in $FZF_PATHS ; do
  if [[ -e $p/.fzf.zsh ]] ; then  # git installation
    . $p/.fzf.zsh
  elif [[ -e $p/completion.zsh && -e $p/key-bindings.zsh ]] ; then
    . $p/completion.zsh
    . $p/key-bindings.zsh
    break
  fi
done

export FZF_DEFAULT_COMMAND='rg --files --hidden'

_fzf_compgen_path() {
  rg --files --hidden . "$"
}

if ! [[ -v SSH_CLIENT ]] ; then  # if this is not a ssh session
  export LIBGL_ALWAYS_INDIRECT=0
  autoload -U set_DISPLAY
  set_DISPLAY
  _gpgbride_dir_win_wslpath="/mnt/c/Users/a/.gpgbridge"
  export SCRIPT_DIR_WSL="$_gpgbride_dir_win_wslpath/repo"
  export PIDFILE_WSL="$HOME/.gpgbridge_wsl.pid"
  export LOGFILE_WSL="$HOME/.gpgbridge_wsl.log"
  export LOGFILE_WIN="$_gpgbride_dir_win_wslpath/gpgbridge_win.log"
  export PIDFILE_WIN="$_gpgbride_dir_win_wslpath/gpgbridge_win.pid"
  . $DOTFILES/deps/gpg-bridge-wsl2-ruby/gpgbridge_helper.sh
  start_gpgbridge --ssh --wsl2
fi

# Entrypoints to ROS
if [[ -e /opt/ros/melodic/setup.zsh ]] ; then
  . /opt/ros/melodic/setup.zsh
  CATKIN_WS=~/catkin_ws
  [[ -e $CATKIN_WS/devel/setup.zsh ]] && . $CATKIN_WS/devel/setup.zsh
fi

# Syntax highlightening for less
export PAGER="less"
export LESS=" -R"
if [[ -e /usr/bin/src-hilite-lesspipe.sh ]] ; then
    export LESSOPEN="| /usr/bin/src-hilite-lesspipe.sh %s"
fi

