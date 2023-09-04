(( $+commands[tmux] ))                         \
    && [[ ! -v TMUX ]]                         \
    && [[ ! -v INSIDE_EMACS ]]                 \
    && [[ $XDG_SESSION_DESKTOP != "sway" ]]    \
    && tmux new-session -A -s $USER-tmux-session -t $USER-tmux-session-group

# Local Variables:
# mode: sh
# sh-shell: zsh
# End:
