PURE_CMD_MAX_EXEC_TIME=60
PURE_GIT_PULL=0
PURE_PROMPT_SYMBOL='%f%F{red}#%f %F{magenta}❯' # prefix prompt when root

print() {
  [ 0 -eq $# -a "prompt_pure_precmd" = "${funcstack[-1]}" ] || builtin print "$@";
}
