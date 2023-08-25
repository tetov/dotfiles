if [[ -v TMUX ]] ; then
    tmux attach-session \
        -t $USER \
        -f active-pane \
        || tmux new-session -s $USER
fi

# Local Variables:
# mode: sh
# sh-shell: zsh
# End:
