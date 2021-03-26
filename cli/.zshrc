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

source ~/.zinit/bin/zinit.zsh

zinit ice atclone"dircolors -b LS_COLORS > clrs.zsh" \
    atpull'%atclone' pick"clrs.zsh" nocompile'!' \
    atload'zstyle ":completion:*" list-colors “${(s.:.)LS_COLORS}”'
zinit light trapd00r/LS_COLORS

zinit wait for \
    atinit"zicompinit; zicdreplay" \
    zdharma/fast-syntax-highlighting \
    atload"_zsh_autosuggest_start" \
    zsh-users/zsh-autosuggestions \
    blockf atpull'zinit creinstall -q .' \
    zsh-users/zsh-completions

zinit load changyuheng/fz

# Load the pure theme, with zsh-async library that's bundled with it.
zinit ice pick"async.zsh" src"pure.zsh"
zinit light sindresorhus/pure

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

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'

export KEYTIMEOUT=1

# virtualenvwrapper
export WORKON_HOME=~/.virtualenvs
export PROJECT_HOME=~/l-repos

VW_PATHS=( /usr/bin/virtualenvwrapper.sh \
           /usr/share/virtualenvwrapper/virtualenvwrapper.sh )

# for p in $VW_PATHS ; do
#   if [[ -e $p ]] ; then
#     . $p
#     autoload -U add-zsh-hook auto_workon
#     add-zsh-hook -Uz chpwd auto_workon
#     auto_workon
#     break
#   fi
# done
#
# aliases
alias ls="exa --group-directories-first"
alias cat="bat"

zvm_after_init() {
    # fzf installed from git
    FZF_PATHS=(~ /usr/share/fzf /usr/share/doc/fzf/examples)

    for p in $FZF_PATHS ; do
      if [[ -e $p/.fzf.zsh ]] ; then  # git installation
        . $p/.fzf.zsh
        break
        fzf_env_settings
      elif [[ -e $p/completion.zsh && -e $p/key-bindings.zsh ]] ; then
        . $p/completion.zsh
        . $p/key-bindings.zsh
        fzf_env_settings
        break
      fi
    done

}

fzf_env_settings() {

    export FZF_DEFAULT_COMMAND='fd --type f --color=always || rg --files --hidden || find .'
    # Use ctrl+o to open selected file(s) in vim
    export FZF_DEFAULT_OPTS="--bind='ctrl-o:execute(vim {})+abort'"
    export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
    # # Using bat as previewer
    export FZF_CTRL_T_OPTS="--preview-window 'right:60%' --preview 'bat --color=always --style=header,grid --line-range :300 {}'"
    # # Changing from ** to ~~ the trigger for autocompletion in shell
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
fi

if [[ -e /opt/ros/noetic/setup.zsh ]] ; then
  . /opt/ros/noetic/setup.zsh
fi

CATKIN_WS=~/catkin_ws
[[ -e $CATKIN_WS/devel/setup.zsh ]] && . $CATKIN_WS/devel/setup.zsh

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
