# man zshzle
bindkey -e

ZKBD_FILE=$ZDOTDIR/.zkbd/$TERM-${${DISPLAY:t}:-$VENDOR-$OSTYPE}

if [[ -e "$ZKBD_FILE" ]] ; then
    . "$ZKBD_FILE"
    [[ -n ${key[Home]} ]] && bindkey "${key[Home]}" beginning-of-line
    [[ -n ${key[End]} ]] && bindkey "${key[End]}" end-of-line
    [[ -n ${key[Delete]} ]] && bindkey "${key[Delete]}" delete-char
else
    echo "Run zkbd to setup keybindings for terminal emulator."
    echo
    echo '$ZDOTDIR ; autoload zkbd ; zkbd'
fi
