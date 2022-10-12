#!/bin/sh

# Run upgrade once a week
# Weeks since epoch: {{ div (now | unixEpoch) 604800 }}

TPM_PLUGIN_UPGRADE_SCRIPT="$XDG_CONFIG_HOME/tmux/plugins/tpm/bin/update_plugins"

[ -e "$TPM_PLUGIN_UPGRADE_SCRIPT" ] && "$TPM_PLUGIN_UPGRADE_SCRIPT" all

ZCOMET_SCRIPT="$XDG_CONFIG_HOME/zsh/.zcomet/bin/zcomet.zsh"

if [ -e "$ZCOMET_SCRIPT" ] ; then
    zsh -c "source $ZCOMET_SCRIPT && zcomet update"
fi
