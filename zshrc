setopt HIST_IGNORE_DUPS

export PATH="$HOME/.bin:$PATH" 

source ~/.antigen/antigen.zsh

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
#antigen bundle git
#antigen bundle osx
#antigen bundle pip
antigen bundle textmate
#antigen bundle command-not-found
#antigen bundle alias-tips

if [ "$OSTYPE"="darwin11.0" ]; then
  antigen-bundle osx
fi

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting

# Load the theme.
antigen theme robbyrussell

# Tell antigen that you're done.
antigen apply

#homebrew
export HOMEBREW_GITHUB_API_TOKEN=36525685ffae44acd45e20eee28274c71da5f2a9
