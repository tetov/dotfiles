tmux-window-name() {
    if [[ -v TMUX_PLUGIN_MANAGER_PATH ]] ; then
        ($TMUX_PLUGIN_MANAGER_PATH/tmux-window-name/scripts/rename_session_windows.py &)
    fi
}

add-zsh-hook chpwd tmux-window-name
