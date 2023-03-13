if test "$INSIDE_EMACS" = 'vterm' \
        -a -n "$EMACS_VTERM_PATH" \
        -a -f "$EMACS_VTERM_PATH/etc/emacs-vterm.fish"
    source
end
