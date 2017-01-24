setopt HIST_IGNORE_DUPS

export PATH="$HOME/.bin:$PATH" 

ZSHA_BASE=$HOME/tetov-dotfiles/zsh
source $ZSHA_BASE/antigen/antigen.zsh
source $ZSHA_BASE/aliases.zsh

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git
antigen bundle textmate
#antigen bundle command-not-found
antigen bundle djui/alias-tips

if [ "$OSTYPE"="darwin11.0" ]; then
  antigen-bundle osx
fi

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting

# Load the theme.
antigen theme avit

# Tell antigen that you're done.
antigen apply