export VISUAL=vim
export EDITOR=$VISUAL
export PAGER=less

export LANG=en_US.UTF-8
export LC_COLLATE=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LC_MESSAGES=en_US.UTF-8
export LC_MONETARY=sv_SE.UTF-8
export LC_NUMERIC=sv_SE.UTF-8
export LC_TIME=sv_SE.UTF-8

export XDG_CONFIG_HOME
export XDG_DATA_HOME=~/.local/share
export XDG_STATE_HOME=~/.local/state
export XDG_RUNTIME_DIR=/run/user/$(id -u)

export DOTFILES=~/dotfiles

export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"

typeset -aU path

path=(/usr/lib/ccache $path)

path+=~/bin
path+=${DOTFILES}/bin
path+=~/.cargo/bin
path+=~/.local/bin
path+=~/.poetry/bin

fpath+="$ZDOTDIR/funcs"
fpath+="$ZDOTDIR/completions"

source ~/.local_env
