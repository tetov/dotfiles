(( $+commands[tmux] ))                         \
    && [[ ! -v TMUX ]]                         \
    && [[ ! -v INSIDE_EMACS ]]                 \
    && [[ ! -v WT_SESSION ]]                   \
    && [[ $TERM_PROGRAM != "vscode" ]]         \
    && [[ $TERM != "dumb" ]]                   \
    && [[ $XDG_SESSION_DESKTOP != "sway" ]]    \
    && tmux new-session -A -s $USER-tmux-session -t $USER-tmux-session-group

# Local Variables:
# mode: sh
# sh-shell: zsh
# End:
