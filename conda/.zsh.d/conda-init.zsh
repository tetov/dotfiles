# set CONDA_ROOT in ~/.local_env

if [[ -v CONDA_ROOT ]] ; then
    __conda_setup="$("$CONDA_ROOT/bin/conda" 'shell.zsh' 'hook' 2> /dev/null)"
    if [ $? -eq 0 ]; then
        eval "$__conda_setup"
    else
        if [ -f "$CONDA_ROOT/etc/profile.d/conda.sh" ]; then
            . "$CONDA_ROOT/etc/profile.d/conda.sh"
        else
            path+="$CONDA_ROOT/bin"
        fi
    fi
    unset __conda_setup
fi

