(( $+commands[tmux] ))                         \
    && [[ ! -v TMUX ]]                         \
    && [[ $XDG_SESSION_DESKTOP != "sway" ]]    \
    && tmux new-session -A -s $USER-tmux-session -t $USER-tmux-session-group

# Local Variables:
# mode: sh
# sh-shell: zsh
# End:
