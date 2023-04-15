# Syntax highlightening for less
export LESS=" -R"
[ -e /usr/bin/src-hilite-lesspipe.sh ] && export LESSOPEN="| /usr/bin/src-hilite-lesspipe.sh %s"
