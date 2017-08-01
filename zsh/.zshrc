export VISUAL=vim
export EDITOR="$VISUAL"

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

setopt HIST_IGNORE_DUPS

dotfiles_dir=$HOME/tetov-dotfiles

path+=$dotfiles_dir/bin

LS_COLORS='no=00;37:fi=00:di=00;33:ln=04;36:pi=40;33:so=01;35:bd=40;33;01:'
export LS_COLORS
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Load variables containing server adresses and user names
source $dotfiles_dir/bin/source_confidentials

ZSHA_BASE=$dotfiles_dir/antigen
source $ZSHA_BASE/antigen.zsh

antigen use oh-my-zsh

antigen bundle command-not-found
antigen bundle zsh-users/zsh-completions

antigen theme gentoo

antigen apply

#Platform independent aliases

alias ll='ls -lh'
alias la='ls -A'
alias l='ls'
alias mkdir='mkdir -pv'
alias rmi='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias ..='cd ..'
alias ofd='open_command $PWD'

if [[ $OSTYPE == darwin* ]] ; then
    export HOMEBREW_NO_ANALYTICS=1

	path+=/usr/local/sbin
    path+="$HOME/.rbenv/bin:$PATH"

    eval "$(rbenv init -)"

    alias ls='ls -FG'
    alias grep='ggrep -i --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn}'

    alias trrr='mv -v ~/Downloads/*.torrent $dropboxPath/trrtz/rtorrent/'
    alias lsrtr='ssh -qt $server_user@$server_url "ls -tr $server_media_dir | tail -n10"'

    alias arka='ssh $server_user@$server_url'

    if groups $whoami | grep &>/dev/null "\badmin\b" ; then
        alias brww='brew update && brew upgrade && brew cleanup && brew doctor && brew cask cleanup && brew cask doctor'
    fi
elif [ "$OSTYPE"="linux-gnu" ]; then
    source $dotfiles_dir/zsh-support/zsh_linux.rc
    path+=$HOME/bin

    alias ls='ls -F --color=auto'
    alias grep='grep -i --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn}'

    alias rthot="watch -n30 'rtcontrol -rs up,down,name xfer=+0 2>&1'"
    alias rtmsg="rtcontrol -s alias,message,name 'message=?*' 'message=!*Tried?all?trackers*'"
    alias rtmsgstats="rtcontrol -q -s alias,message -o alias,message 'message=?*' 'message=!*Tried?all?trackers*' | uniq -c"
    alias rt2days="rtcontrol --column-headers -scompleted -ocompletion completed=-2d"
    alias rtstats="rtcontrol -q -o alias -s alias \\* | uniq -c"
    alias rti="rtcontrol is_complete=no"

    alias lsrtr='ls -1rt $server_media_dir | tail -15'

    alias nohard='find $server_plex_dir -type f ! \( -name "*.jpg" -o -name "*.png" -o -name "*.nfo" -o -name "*.srt" -o -name "amc.excludes" -o -name "*.sub" \) -links 1 -exec du -bh {} \; | sort -h'

    alias trippy='sudo tripwire -m c -I'
    alias trippyaf='sudo apt-get update && sudo apt-get upgrade -y && sudo tripwire -m c -I'

    alias rttrump='rtcontrol -s alias,message,name message="*nregistered*"'
else
	echo 'OSTYPE unknown'
fi

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
