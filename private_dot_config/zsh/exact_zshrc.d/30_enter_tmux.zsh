if [[ ! -v TMUX ]] ; then
    tmux attach-session -t $USER \
        || tmux new-session -s $USER
fi

# Local Variables:
# mode: sh
# sh-shell: zsh
# End:
