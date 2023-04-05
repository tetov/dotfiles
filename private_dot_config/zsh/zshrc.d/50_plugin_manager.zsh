# ZIM
ZIM_HOME=$XDG_CACHE_HOME/zsh/zim
zstyle ':zim' disable-version-check yes
zstyle ':zim:completion' dumpfile ${ZIM_HOME}/zcompdump
zstyle ':completion::complete:*' cache-path ${XDG_CACHE_HOME}/zsh/zcompcache
# Install missing modules, and update ${ZIM_HOME}/init.zsh if missing or outdated.
if [[ ! ${ZIM_HOME}/init.zsh -nt ${ZDOTDIR:-${HOME}}/.zimrc ]]; then
  source ${ZIM_HOME}/zimfw.zsh init -q
fi
source ${ZIM_HOME}/init.zsh
