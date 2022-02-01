export VISUAL=vim
export EDITOR=$VISUAL
export PAGER=less

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

export XDG_CONFIG_HOME
export XDG_DATA_HOME=${XDG_DATA_HOME:=~/.local/share}
export XDG_STATE_HOME=${XDG_STATE_HOME:=~/.local/state}
export XDG_RUNTIME_DIR=${XDG_RUNTIME_DIR}:=/run/user/$(id -u)

export DOTFILES=${DOTFILES:=${HOME}/dotfiles}

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

