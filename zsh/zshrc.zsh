setopt HIST_IGNORE_DUPS

ZSHA_BASE=$HOME/tetov-dotfiles/zsh
source $ZSHA_BASE/antigen/antigen.zsh

git config --global user.email "sonya.mamurin@gmail.com"
git config --global user.name "Sonya Mamurin"

LS_COLORS='no=00;37:fi=00:di=00;33:ln=04;36:pi=40;33:so=01;35:bd=40;33;01:'
export LS_COLORS
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git
antigen bundle textmate
#antigen bundle command-not-found
antigen bundle djui/alias-tips

if [ "$OSTYPE"="darwin16.3.0" ]; then
	antigen-bundle osx
	source $ZSHA_BASE/rc-mac.zsh
	export PATH="$HOME/.bin:$PATH" 
	
elif [ "$OSTYPE"="linux-gnu" ]; then
		source $ZSHA_BASE/rc-linux.zsh

else
	echo 'OSTYPE is wrong'

fi

if [ `whoami` = "gudrun" ]; then
	source ~/.githubtoken
fi

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting

# Load the theme.
antigen theme gentoo

PROMPT='%(!.%{$fg[red]%}.%{$fg[green]%}%n@)%m %{$fg[blue]%}%(!.%1~.%~) $(git_prompt_info)%_$(prompt_char)%{$reset_color%} '

# Tell antigen that you're done.
antigen apply

#Generella alias
alias ls='ls -FG'
alias ll='ls -lh'
alias la='ls -A'
alias l='ls'
alias whatmp3='whatmp3 --skipgenre'
alias ..='cd ..'
alias cd..='cd ..'