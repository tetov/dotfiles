# tetov-dotfiles

My dotfiles. Install with stow, see [stow.sh](./stow.sh) for example usage. 

[More info about using dotfiles with stow](https://taihen.org/managing-dotfiles-with-gnu-stow/).

## Setup

```sh
$ sudo chsh --shell=/bin/zsh $USER
$ ./stow.sh
$ ./update-repos
```

## Update

```sh
$ ./update-repos
```

## TODO
*   write setup-script for debian

## Configuration
*   ZSH with [Antigen](https://github.com/zsh-users/antigen)
*   [Hammerspoon](https://github.com/Hammerspoon/hammerspoon) setup
*   [filebot](https://github.com/filebot/filebot) scripts for use with [plex](https://github.com/plexinc/plex-media-player)
*   scripts for transcoding audio files
*   scripts for rsync
*   [script](./bin/mashpodder-script) for [mashpodder](https://github.com/chessgriffin/mashpodder)
*   [setup script](./install_mac.sh) for Mac

## References I use often

### OS X security
*   [drduh/macOS-Security-and-Privacy-Guide](https://github.com/drduh/macOS-Security-and-Privacy-Guide)
*   [kristovatlas/osx-config-check](https://github.com/kristovatlas/osx-config-check)

### Text edting
*   [Markdown-Cheatsheet](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet)
*   [VIM cheat sheet](https://vim.rtorr.com/)
*   [Markdown to InDesign with Pandoc (via ICML)](http://networkcultures.org/digitalpublishing/2014/10/08/markdown-to-indesign-with-pandoc-via-icml/)
*   [From Word to Markdown to InDesign](http://rhythmus.be/md2indd/)

### Specific programs
*   [rclone usage](http://rclone.org/docs/)
*   [rtcontrol usage](https://github.com/pyroscope/pyrocore/blob/ef1537281e075e1d8a8956390e9164162db9e0a9/docs/usage-rtcontrol.rst)
*   [httpie cheat sheet](http://ricostacruz.com/cheatsheets/httpie.html)
*   [find cheat sheet](http://ricostacruz.com/cheatsheets/find.html)
*   [sed cheat sheet](http://ricostacruz.com/cheatsheets/sed.html)
*   [shell scripting cheat sheet](http://ricostacruz.com/cheatsheets/sh.html)
*   [BashGuide](http://mywiki.wooledge.org/BashGuide)
*   [zsh cheat sheet](http://ricostacruz.com/cheatsheets/zsh.html)
*   [rename cheat sheet](http://ricostacruz.com/cheatsheets/rename.html)
*   [curl cheat sheet](http://ricostacruz.com/cheatsheets/curl.html)
*   [rsync cheat sheet](http://ricostacruz.com/cheatsheets/rsync.html)
*   [tmux cheat sheet](http://tmuxcheatsheet.com/)
### Other
*   [emoji cheat sheet](http://www.webpagefx.com/tools/emoji-cheat-sheet/)
