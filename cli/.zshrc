# Source general shell env
export VISUAL=vim
export EDITOR="$VISUAL"

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export XDG_RUNTIME_DIR=/run/user/$(id -u)

export DOTFILES=$HOME/tetov-dotfiles
export ZSH_DIR="$HOME/.zsh"

_source_if_exists() {
    [[ -e "$1" ]] && source "$1"
}

_source_if_exists ~/.local_env

# Path
typeset -U path
path=(~/bin $DOTFILES/bin $path)

path+=~/.cargo/bin

fpath+="$ZSH_DIR/funcs"

# history settings
HISTFILE="~/.zsh_history"
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

source "$ZSH_DIR/zinit-setup.zsh"

if ! [[ -v SSH_CLIENT ]] ; then  # if this is not a ssh session
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
fi

if [[ -v ROS_DIR ]] ; then
    # Entrypoints to ROS
    source $ROS_DIR/setup.zsh
    _source_if_exists $CATKIN_WS/devel/setup.zsh
fi

# Syntax highlightening for less
export PAGER="less"
export LESS=" -R"
if [[ -e /usr/bin/src-hilite-lesspipe.sh ]] ; then
    export LESSOPEN="| /usr/bin/src-hilite-lesspipe.sh %s"
fi

# to make sure yarn/node-gyp doesn't use appdata dir
# https://paulochaves.dev/blog/updating-gatsby-with-sharp-dependency-errors-on-ubuntu-wsl
unset APPDATA

NOCONDA_PATH=$PATH

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/opt/anaconda/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/opt/anaconda/etc/profile.d/conda.sh" ]; then
        . "/opt/anaconda/etc/profile.d/conda.sh"
    else
        export PATH="/opt/anaconda/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

source $ZSH_DIR/aliases.zsh
