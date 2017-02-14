setopt HIST_IGNORE_DUPS

ZSHA_BASE=$HOME/.antigen
source $ZSHA_BASE/antigen.zsh

LS_COLORS='no=00;37:fi=00:di=00;33:ln=04;36:pi=40;33:so=01;35:bd=40;33;01:'
export LS_COLORS
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git
antigen bundle textmate

if [[ $OSTYPE == darwin* ]] ; then
	antigen-bundle osx
	source $HOME/.zsh_mac.rc
	path+=/usr/local/sbin
elif [ "$OSTYPE"="linux-gnu" ]; then
	source $HOME/.zsh_linux.rc
else
	echo 'OSTYPE is wrong'
fi

#Platform independent aliases

alias ll='ls -lh'
alias la='ls -A'
alias l='ls'
alias l.='ls -d .*'
alias cd..='cd ..'
alias mkdir='mkdir -pv'
alias rmi='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias ..='cd ..'

alias bare='git --git-dir=$HOME/.baredotfiles/ --work-tree=$HOME $@'

alias watch='watch -n30 '

alias whatmp3='whatmp3 --skipgenre'
alias whatmp3n='whatmp3 --V0 -n '

alias dithr='mkdir ../"${PWD##*/}_16"; for flac in *.flac; do sox -S "${flac}" -G -b 16 ../"${PWD##*/}_16"/"${flac}" rate -v -L 48000 dither; done'

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting

# Load the theme.
antigen theme gentoo

# Tell antigen that you're done.
antigen apply

PROMPT='%(!.%{$fg[red]%}.%{$fg[green]%}%n@)%m %{$fg[blue]%}%(!.%1~.%~) $(git_prompt_info)%_%{$fg[white]%}$(prompt_char)%{$reset_color%} '

dropboxPath=$(jq -r .personal.path .dropbox/info.json)
if command -v jq &&  [ -f $HOME/.dropbox/info.json ] ; then
	export dropboxPath=$(jq -r .personal.path .dropbox/info.json)
else
	export dropboxPath=$(cat $HOME/.altDropboxPath)

source $dropboxPath/.confidentials

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

function iterm2_print_user_vars() {
	iterm2_set_user_var humpday $(is_it_wednesday)
}

function is_it_wednesday {
	if [[ $(date +%A) = "Wednesday" ]]; then
    	echo "🐪" # Camel Prompt
	elif [[ $(date +%A) = "Thursday" ]]; then
		echo "🐝" # Bee Prompt
	elif [[ $(date +%A) = "Friday" ]]; then
		echo "👽" # Alien Prompt
	elif [[ $(date +%A) = "Saturday" ]]; then
		echo "🐈" # Caturday
    else
    	echo "🐙" # Inky Prompt
  fi
}
