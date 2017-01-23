setopt HIST_IGNORE_DUPS

export PATH="$HOME/.bin:$PATH" 

ZSHA_BASE=$HOME/.zsh-antigen
source $ZSHA_BASE/antigen/antigen.zsh
source $ZSHA_BASE/aliases.zsh

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
#antigen theme agnoster
#antigen theme robbyrussell
antigen theme https://github.com/caiogondim/bullet-train-oh-my-zsh-theme bullet-train

BULLETTRAIN_PROMPT_SEPARATE_LINE=false
BULLETTRAIN_TIME_SHOW=false
BULLETTRAIN_VIRTUALENV_SHOW=false
BULLETTRAIN_RUBY_SHOW=false
BULLETTRAIN_DIR_EXTENDED=0
BULLETTRAIN_HG_SHOW=0

BULLETTRAIN_PROMPT_ORDER=(
	status
#	context
	dir
	git
)

# Tell antigen that you're done.
antigen apply

#homebrew
export HOMEBREW_GITHUB_API_TOKEN=36525685ffae44acd45e20eee28274c71da5f2a9

export TERM="xterm-256color"
