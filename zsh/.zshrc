# ENV

export VISUAL=vim
export EDITOR="$VISUAL"

export LC_ALL=en_US.UTF-8  
export LANG=en_US.UTF-8

setopt HIST_IGNORE_DUPS

dotfiles_dir=$HOME/tetov-dotfiles

path+=$dotfiles_dir/bin

ZSHA_BASE=$dotfiles_dir/antigen
source $ZSHA_BASE/antigen.zsh

LS_COLORS='no=00;37:fi=00:di=00;33:ln=04;36:pi=40;33:so=01;35:bd=40;33;01:'
export LS_COLORS
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git

# zsh-completions https://github.com/zsh-users/zsh-completions
antigen bundle zsh-users/zsh-completions

if [[ $OSTYPE == darwin* ]] ; then
	antigen-bundle osx
fi

# Self-update
antigen bundle unixorn/autoupdate-antigen.zshplugin

antigen theme gentoo

antigen apply

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
alias watch='watch -n30 '
alias whatmp3='whatmp3 --skipgenre'
alias whatmp3n='whatmp3 --V0 -n '
alias dithr='mkdir ../"${PWD##*/}_16"; for flac in *.flac; do sox -S "${flac}" -G -b 16 ../"${PWD##*/}_16"/"${flac}" rate -v -L 48000 dither; done'

if [[ $OSTYPE == darwin* ]] ; then
	source $dotfiles_dir/zsh-support/zsh_mac.rc
	path+=/usr/local/sbin
  path+="$HOME/.rbenv/bin:$PATH"
elif [ "$OSTYPE"="linux-gnu" ]; then
	source $dotfiles_dir/zsh-support/zsh_linux.rc
  path+=$HOME/bin
else
	echo 'OSTYPE unknown'
fi

PROMPT='%(!.%{$fg[red]%}.%{$fg[green]%}%n@)%m %{$fg[blue]%}%(!.%1~.%~) $(git_prompt_info)%_%{$fg[white]%}$(prompt_char)%{$reset_color%} '

source $dotfiles_dir/bin/source_confidentials

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
