ZSHRC_DIR=$ZDOTDIR/zshrc.d

_source_if_exists() {
    [[ -r "$1" ]] && source "$1"
}

_source_if_exists ~/.local_env

# ROS
_source_if_exists $ROS_DIR/setup.zsh

if [[ -v CATKIN_WS ]] ; then
    _source_if_exists $CATKIN_WS/devel/setup.zsh
    _source_if_exists $CATKIN_WS/devel_isolated/setup.zsh
fi

_source_if_exists /usr/share/nvm/init-nvm.sh
_source_if_exists ~/.opam/opam-init/init.zsh

# history settings
HISTFILE=$XDG_STATE_HOME/zsh/history
HISTSIZE=50000
SAVEHIST=10000

setopt extended_history       # record timestamp of command in HISTFILE
setopt hist_ignore_dups       # ignore duplicated commands history list
setopt hist_ignore_space      # ignore commands that start with space
setopt hist_reduce_blanks # remove superfluous blanks from history items
setopt hist_verify            # show command with history expansion to user before running it
setopt inc_append_history     # add commands to HISTFILE in order of execution
setopt share_history          # share command history dat

setopt auto_cd # cd by typing directory name if it's not a command
setopt complete_in_word # complete where cursor is
setopt always_to_end # always put cursor at end after completing

zstyle ':completion:::::' completer _expand _complete _ignored _approximate #enable approximate matches for completion

# For gpg agent forwarding. (Dir gets deleted on log out)
gpgconf --create-socketdir

unset SSH_AGENT_PID
if [[ ! -v SSH_CLIENT ]] && [[ -v SSH_TTY ]]; then
    export GPG_TTY=$(tty)
fi

gpgconf --launch gpg-agent

if [[ $(uname -r) =~ [Mm]icrosoft ]] ; then  # if this is WSL
  export LIBGL_ALWAYS_INDIRECT=0
  autoload -U set_DISPLAY
  set_DISPLAY
  _gpgbride_dir_win_wslpath="$WINHOME/.gpgbridge"
  export SCRIPT_DIR_WSL="$_gpgbride_dir_win_wslpath/repo"
  export PIDFILE_WSL="$HOME/.gpgbridge_wsl.pid"
  export LOGFILE_WSL="$HOME/.gpgbridge_wsl.log"
  export LOGFILE_WIN="$_gpgbride_dir_win_wslpath/gpgbridge_win.log"
  export PIDFILE_WIN="$_gpgbride_dir_win_wslpath/gpgbridge_win.pid"
  . $DOTFILES/deps/gpg-bridge-wsl2-ruby/gpgbridge_helper.sh
  start_gpgbridge --ssh --wsl2

  export BROWSER="${BROWSER:-wsl-open}"
fi

# Syntax highlightening for less
export LESS=" -R"
if [[ -e /usr/bin/src-hilite-lesspipe.sh ]] ; then
    export LESSOPEN="| /usr/bin/src-hilite-lesspipe.sh %s"
fi

# ZCOMET

zcomet_dir=$ZDOTDIR/.zcomet

# Clone zcomet if necessary
if [[ ! -f $zcomet_dir/bin/zcomet.zsh ]]; then
  command git clone https://github.com/agkozak/zcomet.git $zcomet_dir/bin
fi

source $zcomet_dir/bin/zcomet.zsh

zcomet load sindresorhus/pure async.zsh pure.zsh
PURE_CMD_MAX_EXEC_TIME=60
PURE_GIT_PULL=0
# Make pure theme single line
prompt_newline='%666v'
PROMPT=" $PROMPT"

# zstyle :prompt:pure:user color green
# zstyle :prompt:pure:host color green
# zstyle :prompt:pure:git:branch color white
zstyle :prompt:pure:git:stash show yes

export NVM_COMPLETION=true
export NVM_LAZY_LOAD=false
export NVM_AUTO_USE=true
export NVM_DIR="$HOME/.nvm"
zcomet load agkozak/zsh-z

zcomet load zsh-users/zsh-completions

# update tmux title
zcomet snippet https://raw.githubusercontent.com/jreese/zsh-titles/master/titles.plugin.zsh

zcomet load zdharma-continuum/fast-syntax-highlighting

zcomet load jeffreytse/zsh-vi-mode

zcomet compinit

for file in "$ZSHRC_DIR"/*.zsh; do
    source "$file"
done

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/opt/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/opt/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/opt/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/opt/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

# Anaconda's CA bundle gets picket up by curl
[[ -e /etc/ssl/certs/ca-certificates.crt ]] && CURL_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt
[[ -e /etc/ssl/certs/ca-bundle.crt ]] && CURL_CA_BUNDLE=/etc/ssl/certs/ca-bundle.crt