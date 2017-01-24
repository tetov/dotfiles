export EDITOR=/usr/bin/nano

export LANG=en_US.UTF-8
export LC_ADDRESS=sv_SE.UTF-8
export LC_COLLATE=sv_SE.UTF-8
export LC_CTYPE=sv_SE.UTF-8
export LC_MONETARY=sv_SE.UTF-8
export LC_MEASUREMENT=sv_SE.UTF-8
export LC_MESSAGES=en_US.UTF-8
export LC_NUMERIC=sv_SE.UTF-8
export LC_PAPER=sv_SE.UTF-8
export LC_RESPONSE=en_US.UTF-8
export LC_TELEPHONE=sv_SE.UTF-8
export LC_TIME=sv_SE.UTF-8

#my aliases for arka

alias ls='ls -F --color=auto'
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

alias topu='top -U farligm'
alias whatmp3='whatmp3 --skipgenre'

alias rthot="watch -n30 'rtcontrol -rs up,down,name xfer=+0 2>&1'"
alias rtmsg="rtcontrol -s alias,message,name 'message=?*' 'message=!*Tried?all?trackers*'"
alias rtmsgstats="rtcontrol -q -s alias,message -o alias,message 'message=?*' 'message=!*Tried?all?trackers*' | uniq -c"
alias rt2days="rtcontrol --column-headers -scompleted -ocompletion completed=-2d"
alias rtstats="rtcontrol -q -o alias -s alias \\* | uniq -c"
alias rti="rtcontrol is_complete=no"

alias watch="watch -n30 "

alias dithr='mkdir ../"${PWD##*/}_16"; for flac in *.flac; do sox -S "${flac}" -G -b 16 ../"${PWD##*/}_16"/"${flac}" rate -v -L 48000 dither; done'
alias prlll='printf "[hide=Spectrograms]" && ls | parallel -k spectro -p && echo "[/hide]"'

alias lsrtr='ls -1rt --color=always ~/bay/vind/rtorrent.data/ | tail -15'

alias slog='sudo tail -50 /var/log/syslog'

alias nohard='find /home/farligm/v/plex.bibl/ -type f ! \( -name "*.jpg" -o -name "*.png" -o -name "*.nfo" -o -name "*.srt" -o -name "amc.excludes" -o -name "*.sub" \) -links 1 -exec du -bh {} \; | sort -h'

alias trippy='sudo tripwire -m c -I'
alias trippyaf='sudo apt-get update && sudo apt-get upgrade -y && sudo tripwire -m c -I'

alias rttrump='rtcontrol -s alias,message,name message="*nregistered*"'

alias rclone='sudo rclone'