# Source general shell env
. $HOME/.shellrc

export PS1='\u@\h \w \$ '

if [ -f "$(brew --prefix)"/etc/bash_completion >/dev/null 2>&1 ]; then
    . "$(brew --prefix)"/etc/bash_completion
fi
