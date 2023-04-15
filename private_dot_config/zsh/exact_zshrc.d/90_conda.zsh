# /etc/profile.d/conda.sh available from conda.rpm install
if [ -e /etc/profile.d/conda.sh  ] ; then
    . /etc/profile.d/conda.sh
elif (( $+commands[conda] )) ; then
    eval $(conda shell.zsh hook)
fi
