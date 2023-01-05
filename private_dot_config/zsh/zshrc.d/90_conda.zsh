# /etc/profile.d/conda.sh available from conda.rpm install
test -e /etc/profile.d/conda.sh && . /etc/profile.d/conda.sh
# (( $+commands[conda] )) && eval $(conda shell.zsh hook)
