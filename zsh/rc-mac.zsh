#Inställningar
export PATH="$HOME/.bin:$PATH" 
export EDITOR="/usr/local/bin/mate -w"

#my aliases

# Koperiat och redigerat från ~/.bash_profile 20161002

alias ls='ls -FG'
alias ll='ls -lh'
alias la='ls -A'
alias l='ls'
alias whatmp3='whatmp3 --skipgenre'
alias ..='cd ..'
alias cd..=' cd ..'

alias arka='ssh farligm@a.tetov.se'
alias arktun='ssh -D 12345 farligm@a.tetov.se'

alias dithr='mkdir ../"${PWD##*/}_16"; for flac in *.flac; do sox -S "${flac}" -G -b 16 ../"${PWD##*/}_16"/"${flac}" rate -v -L 48000 dither; done'

alias trrr='mv -v /Users/farligmidsommar/Downloads/*.torrent /Volumes/Anathema/Dropbox/trrtz/rtorrent/'

alias whatmp3n='whatmp3 --V0 -n '

#Nya till ZSH

alias zshconfig="mate ~/.zshrc"

alias lg='login gudrun'

alias brww='brew update && brew upgrade && brew cleanup && brew doctor && brew cask update && brew cask cleanup && brew cask doctor'

#The first is f, which searches the current directory subtree for files with names containing a string (ignoring case). f png would find all PNG files in the current subtree, as well as “PNGisMyFavorite.txt” and so forth.

#The second is r, which recursively greps the current directory subtree for files matching a pattern. r HTTP would grep for files containing that exact string, while r '"http[^"]*"' -i would search for double-quoted strings starting with “http”, ignoring case.

alias f="ag -g"
alias r=ag