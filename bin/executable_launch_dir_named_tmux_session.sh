#!/usr/bin/env sh

# # Construct the path for the folder icon
# FOLDER_ICON_PATH="$folder/.icon"

# if [ -f $FOLDER_ICON_PATH ]; then
#     folder_icon="$(cat $FOLDER_ICON_PATH)"
# else
#     echo "Could not find an .icon file in $folder, using the default icon ️️️️️️🖥️"
#     folder_icon="🖥️"
# fi

# Strip the path and leave the folder name
dir_name="$(basename "$(pwd)")"

# Construct the session name
# SESSION="$folder_icon  $folder_name"
SESSION="$dir_name"

# Attach to Tmux

if [ -z "$TMUX" ]; then
    # We're not inside Tmux
    echo "Attaching to $SESSION"
    tmux attach-session -d -t "$SESSION" || tmux new-session -s "$SESSION"
else
    # We're inside Tmux
    if ! tmux has-session -t "$SESSION" ; then
        # Create a new dettached session
        tmux new -s "$SESSION" -d
    fi

    # Switch to the session
    tmux switch-client -t "$SESSION"

fi
