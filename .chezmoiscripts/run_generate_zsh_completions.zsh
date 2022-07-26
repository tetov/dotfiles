#!/usr/bin/env zsh

chezmoi completion zsh --output=$XDG_CONFIG_HOME/zsh/completions/_chezmoi

(( $+commands[poetry] )) && poetry completions zsh > $XDG_CONFIG_HOME/zsh/completions/_poetry
(( $+commands[op] )) && op completion zsh > $XDG_CONFIG_HOME/zsh/completions/_op

exit 0
