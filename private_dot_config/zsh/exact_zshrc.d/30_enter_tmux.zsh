[[ -v TMUX ]] || \
    tmux new-session -A -s $USER-tmux-session -t $USER-tmux-session-group

# Local Variables:
# mode: sh
# sh-shell: zsh
# End:
