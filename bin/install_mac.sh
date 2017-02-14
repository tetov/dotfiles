################################################################################
## Script for new installation on mac
## Do not execute, run line by line.
##
## Sources
## https://github.com/drduh/macOS-Security-and-Privacy-Guide
## https://github.com/atomantic/dotfiles/blob/master/install.sh
##
################################################################################

/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

curl -Lks https://gist.githubusercontent.com/sonyamamurin/1bc7caeb3c4b842da302bdb155bc8d52/raw/afff4077f6234f0680d06422a1b5e13ff68280cb/install-baredotfiles.sh | /bin/bash
# Eller kör manuellt från https://gist.github.com/sonyamamurin/1bc7caeb3c4b842da302bdb155bc8d52

brew update
brew upgrade

brew install bash-completion
brew install duti
brew install fdupes
brew install findutils
brew install git
brew install googler
brew install homebrew/boneyard/lftp
brew install jq
brew install lynx
brew install mackup
brew install mas
brew install md5sha1sum
brew install media-info
brew install minisign
brew install mktorrent
brew install ncdu
brew install nmap
brew install openssl
brew install pandoc
brew install privoxy
brew install python
brew install python3
brew install rclone
brew install rename
brew install sox
brew install speedtest_cli
brew install spoof-mac
brew install homebrew/fuse/sshfs
brew install sqlite
brew install svtplay-dl
brew install the_silver_searcher
brew install tmux
brew install tor
brew install transmission
brew install tree
brew install unrar
brew install watch
brew install wget
brew install youtube-dl
brew install z
brew install zsh
brew install zsh-completions

# set zsh as the user login shell
CURRENTSHELL=$(dscl . -read /Users/$USER UserShell | awk '{print $2}')
if [[ "$CURRENTSHELL" != "/usr/local/bin/zsh" ]]; then
  # "setting newer homebrew zsh (/usr/local/bin/zsh) as your shell (password required)"
  # sudo bash -c 'echo "/usr/local/bin/zsh" >> /etc/shells'
  # chsh -s /usr/local/bin/zsh
  sudo dscl . -change /Users/$USER UserShell $SHELL /usr/local/bin/zsh > /dev/null 2>&1
  ok
fi

brew tap caskroom/cask

brew cask install 1password
brew cask install adobe-digital-editions
brew cask install adobe-reader
brew cask install angry-ip-scanner
brew cask install animated-gif-quicklook
brew cask install appcleaner
brew cask install appcleaner
brew cask install betterzipql
brew cask install cheatsheet
brew cask install discord
brew cask install dropbox
brew cask install firefox
brew cask install github-desktop
brew cask install gmail-notifier
brew cask install google-chrome
brew cask install google-drive
brew cask install gpgtools
brew cask install grandperspective
brew cask install hammerspoon
brew cask install iterm2
brew cask install jumpcut
brew cask install karabiner-elements
brew cask install keepingyouawake
brew cask install kindle
brew cask install little-flocker
brew cask install little-snitch
brew cask install mediainfo
brew cask install messenger
brew cask install onyx
brew cask install osxfuse
brew cask install qlcolorcode
brew cask install qlimagesize
brew cask install qlmarkdown
brew cask install qlprettypatch
brew cask install qlstephen
brew cask install qlvideo
brew cask install quicklook-csv
brew cask install quicklook-json
brew cask install quicklook-pat
brew cask install quicklook-pfm
brew cask install quicklookase
brew cask install quicknfo
brew cask install rcdefaultapp
brew cask install santa
brew cask install skype
brew cask install slack
brew cask install spotifree
brew cask install steam
brew cask install suspicious-package
brew cask install syncthing-bar
brew cask install tomighty
brew cask install transmission
brew cask install tuntap
brew cask install virtualbox
brew cask install viscosity
brew cask install vlc
brew cask install vlc-webplugin
brew cask install webpquicklook
brew cask install wineskin-winery
brew cask install xld
brew cask install xquartz

brew tap caskroom/fonts
brew cask install font-fontawesome
brew cask install font-awesome-terminal-fonts
brew cask install font-hack
brew cask install font-inconsolata-dz-for-powerline
brew cask install font-inconsolata-g-for-powerline
brew cask install font-inconsolata-for-powerline
brew cask install font-roboto-mono
brew cask install font-roboto-mono-for-powerline
brew cask install font-source-code-pro

brew cleanup

mackup restore # after logged in to Dropbox

################################################################################
# Start drduh/macOS-Security-and-Privacy-Guide recommendations
# Use admin account
#
################################################################################

# Firmware password
#

# "The firmware password can also be managed with the firmwarepasswd utility while booted into the OS. For example, to prompt for the firmware password when attempting to boot from a different volume:""
sudo firmwarepasswd -setpasswd -setmode command

# MacOS firewall
#

# Enable firewall
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setglobalstate on

# Enable logging
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setloggingmode on

# Host file
# Append and edit /etc/hosts from:
# http://someonewhocares.org/hosts/zero/hosts
# https://github.com/l1k/osxparanoia/blob/master/hosts
# https://github.com/StevenBlack/hosts
# https://github.com/gorhill/uMatrix/blob/master/assets/umatrix/hosts-files.json
#
# To append:
# curl "URL" | sudo tee -a /etc/hosts
#
# To check length:
# wc -l /etc/hosts
#
# To test
# egrep -ve "^#|^255.255.255|^0.0.0.0|^127.0.0.0|^0 " /etc/hosts
#
# Expected output:
# ::1 localhost
# fe80::1%lo0 localhost

# Computer name, host name
#

sudo scutil --set ComputerName your_computer_name
sudo scutil --set LocalHostName your_hostname

# Disable captive portal function
#

sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.captive.control Active -bool false

# Setup privoxy
#

# Start service
sudo brew services start privoxy

# Set http-proxy
sudo networksetup -setwebproxy "Wi-Fi" 127.0.0.1 8118

# Set https-proxy
sudo networksetup -setsecurewebproxy "Wi-Fi" 127.0.0.1 8118

# Confirm proxy set
scutil --proxy

# Expected output:
#<dictionary> {
#  ExceptionsList : <array> {
#    0 : *.local
#    1 : 169.254/16
#  }
#  FTPPassive : 1
#  HTTPEnable : 1
#  HTTPPort : 8118
#  HTTPProxy : 127.0.0.1
#}

# and
ALL_PROXY=127.0.0.1:8118 curl -I http://p.p/

# Expected output:
# HTTP/1.1 200 OK
# Content-Length: 2401
# Content-Type: text/html
# Cache-Control: no-cache

# Edit user.action and config according to backup gist
# https://gist.github.com/sonyamamurin/8b91a9b18452a5692b3ad0e39b06dd86

# Setup spoof-mac
#

# Edit homebrew.mxcl.spoof-mac.plist in /usr/local/Cellar/spoof-mac/x.x.x
# to look like this: (make en1 is the right one, run spoof-mac list)

#<?xml version="1.0" encoding="UTF-8"?>
#<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
#<plist version="1.0">
#  <dict>
#    <key>Label</key>
#    <string>homebrew.mxcl.spoof-mac</string>
#    <key>ProgramArguments</key>
#    <array>
#      <string>/usr/local/opt/spoof-mac/bin/spoof-mac</string>
#      <string>randomize</string>
#      <string>en1</string>
#    </array>
#    <key>RunAtLoad</key>
#    <true/>
#    <key>StandardErrorPath</key>
#    <string>/dev/null</string>
#    <key>StandardOutPath</key>
#    <string>/dev/null</string>
#  </dict>
#</plist>

# Setup santa
# https://github.com/drduh/macOS-Security-and-Privacy-Guide#binary-whitelisting
#

# start lockdown
sudo defaults write /var/db/santa/config.plist ClientMode -int 2

################################################################################
# Start of commands from atomantic/dotfiles
# https://github.com/atomantic/dotfiles/blob/master/install.sh
#
################################################################################

# Enable firewall. Possible values:
#   0 = off
#   1 = on for specific sevices
#   2 = on for essential services
sudo defaults write /Library/Preferences/com.apple.alf globalstate -int 1

# Disable guest account login
sudo defaults write /Library/Preferences/com.apple.loginwindow GuestEnabled -bool false

# SSD specific tweaks
# Disable local Time Machine snapshots
sudo tmutil disablelocal

# Disable hibernation (speeds up entering sleep mode)"
sudo pmset -a hibernatemode 0

# Remove the sleep image file to save disk space
sudo rm -rf /Private/var/vm/sleepimage
# Create a zero-byte file instead
sudo touch /Private/var/vm/sleepimage
# …and make sure it can’t be rewritten
sudo chflags uchg /Private/var/vm/sleepimage
# Menu bar: disable transparency
defaults write NSGlobalDomain AppleEnableMenuBarTransparency -bool false

# Menu bar: hide the Time Machine, Volume, User, and Bluetooth icons"
for domain in ~/Library/Preferences/ByHost/com.apple.systemuiserver.*; do
  defaults write "${domain}" dontAutoLoad -array \
    "/System/Library/CoreServices/Menu Extras/TimeMachine.menu" \
#    "/System/Library/CoreServices/Menu Extras/Volume.menu" \
    "/System/Library/CoreServices/Menu Extras/User.menu"
done;
defaults write com.apple.systemuiserver menuExtras -array \
#  "/System/Library/CoreServices/Menu Extras/Bluetooth.menu" \
  "/System/Library/CoreServices/Menu Extras/AirPort.menu" \
  "/System/Library/CoreServices/Menu Extras/Battery.menu" \
  "/System/Library/CoreServices/Menu Extras/Clock.menu"
ok

# Expand save panel by default
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true

# Expand print panel by default
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true

# Save to disk (not to iCloud) by default
defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false

# Automatically quit printer app once the print jobs complete"
defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true

# Disable the crash reporter"
defaults write com.apple.CrashReporter DialogType -string "none"

# Set Help Viewer windows to non-floating mode"
defaults write com.apple.helpviewer DevMode -bool true

# Restart automatically if the computer freezes"
sudo systemsetup -setrestartfreeze on

# Check for software updates daily, not just once per week"
defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1

# Disable smart quotes as they’re annoying when typing code"
defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false

# Disable smart dashes as they’re annoying when typing code"
defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false

# Increase sound quality for Bluetooth headphones/headsets"
defaults write com.apple.BluetoothAudioAgent "Apple Bitpool Min (editable)" -int 40

# Enable full keyboard access for all controls (e.g. enable Tab in modal dialogs)"
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3

# Disable press-and-hold for keys in favor of key repeat"
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

# Set a blazingly fast keyboard repeat rate"
defaults write NSGlobalDomain KeyRepeat -int 1
defaults write NSGlobalDomain InitialKeyRepeat -int 10

# Disable auto-correct"
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false

# Require password immediately after sleep or screen saver begins"
defaults write com.apple.screensaver askForPassword -int 1
defaults write com.apple.screensaver askForPasswordDelay -int 0

# Save screenshots to the desktop"
defaults write com.apple.screencapture location -string "${HOME}/Desktop"

# Save screenshots in PNG format (other options: BMP, GIF, JPG, PDF, TIFF)"
defaults write com.apple.screencapture type -string "png"

# Disable shadow in screenshots"
defaults write com.apple.screencapture disable-shadow -bool true

# Enable subpixel font rendering on non-Apple LCDs"
defaults write NSGlobalDomain AppleFontSmoothing -int 2

# Enable HiDPI display modes (requires restart)"
sudo defaults write /Library/Preferences/com.apple.windowserver DisplayResolutionEnabled -bool true

################################################################################
# Finder
################################################################################

# Allow quitting Finder via ⌘ + Q; doing so will also hide desktop icons"
defaults write com.apple.finder QuitMenuItem -bool true

# Set ~ as the default location for new Finder windows"
# For other paths, use 'PfLo' and 'file:///full/path/here/'
defaults write com.apple.finder NewWindowTarget -string "PfHm"

# Show hidden files by default
defaults write com.apple.finder AppleShowAllFiles -bool true

# Show all filename extensions
defaults write NSGlobalDomain AppleShowAllExtensions -bool true

# Show status bar
defaults write com.apple.finder ShowStatusBar -bool true

# Show path bar
defaults write com.apple.finder ShowPathbar -bool true

# Allow text selection in Quick Look
defaults write com.apple.finder QLEnableTextSelection -bool true

# Display full POSIX path as Finder window title
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

# When performing a search, search the current folder by default
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

# Disable the warning when changing a file extension
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

# Enable spring loading for directories
defaults write NSGlobalDomain com.apple.springing.enabled -bool true

# Remove the spring loading delay for directories
defaults write NSGlobalDomain com.apple.springing.delay -float 0

# Avoid creating .DS_Store files on network volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

# Automatically open a new Finder window when a volume is mounted
defaults write com.apple.frameworks.diskimages auto-open-ro-root -bool true
defaults write com.apple.frameworks.diskimages auto-open-ro-root -bool true
defaults write com.apple.finder OpenWindowForNewRemovableDisk -bool true

# Use list view in all Finder windows by default
# Four-letter codes for the other view modes: `icnv`, `clmv`, `Flwv`
defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"

# Empty Trash securely by default
defaults write com.apple.finder EmptyTrashSecurely -bool true

# Enable AirDrop over Ethernet and on unsupported Macs running Lion
defaults write com.apple.NetworkBrowser BrowseAllInterfaces -bool true

# Show the ~/Library folder
chflags nohidden ~/Library


# Expand the following File Info panes: “General”, “Open with”, and “Sharing & Permissions”"
defaults write com.apple.finder FXInfoPanesExpanded -dict \
  General -bool true \
  OpenWith -bool true \
  Privileges -bool true

###############################################################################
# "Dock & Dashboard"
###############################################################################

# Enable highlight hover effect for the grid view of a stack (Dock)"
defaults write com.apple.dock mouse-over-hilite-stack -bool true

# Set the icon size of Dock items to 36 pixels"
# defaults write com.apple.dock tilesize -int 36

# Change minimize/maximize window effect to scale"
# defaults write com.apple.dock mineffect -string "scale"

# Minimize windows into their application’s icon"
# defaults write com.apple.dock minimize-to-application -bool true

# Enable spring loading for all Dock items"
defaults write com.apple.dock enable-spring-load-actions-on-all-items -bool true

# Show indicator lights for open applications in the Dock"
defaults write com.apple.dock show-process-indicators -bool true

# Don’t animate opening applications from the Dock
# defaults write com.apple.dock launchanim -bool false

# Speed up Mission Control animations
defaults write com.apple.dock expose-animation-duration -float 0.1

# Don’t group windows by application in Mission Control
# (i.e. use the old Exposé behavior instead)
defaults write com.apple.dock expose-group-by-app -bool false

# Disable Dashboard
defaults write com.apple.dashboard mcx-disabled -bool true

# Don’t show Dashboard as a Space
defaults write com.apple.dock dashboard-in-overlay -bool true

# Don’t automatically rearrange Spaces based on most recent use
defaults write com.apple.dock mru-spaces -bool false

# Remove the auto-hiding Dock delay
# defaults write com.apple.dock autohide-delay -float 0
# Remove the animation when hiding/showing the Dock"
# defaults write com.apple.dock autohide-time-modifier -float 0

# Automatically hide and show the Dock
defaults write com.apple.dock autohide -bool true

# Make Dock icons of hidden applications translucent
defaults write com.apple.dock showhidden -bool true

# Make Dock more transparent
# defaults write com.apple.dock hide-mirror -bool true

# Reset Launchpad, but keep the desktop wallpaper intact
# find "${HOME}/Library/Application Support/Dock" -name "*-*.db" -maxdepth 1 -delete

# Configuring Hot Corners
# Possible values:
#  0: no-op
#  2: Mission Control
#  3: Show application windows
#  4: Desktop
#  5: Start screen saver
#  6: Disable screen saver
#  7: Dashboard
# 10: Put display to sleep
# 11: Launchpad
# 12: Notification Center

# Top left screen corner → Mission Control"
# defaults write com.apple.dock wvous-tl-corner -int 2
# defaults write com.apple.dock wvous-tl-modifier -int 0
# Top right screen corner → Desktop"
# defaults write com.apple.dock wvous-tr-corner -int 4
# defaults write com.apple.dock wvous-tr-modifier -int 0
# Bottom right screen corner → Start screen saver"
# defaults write com.apple.dock wvous-br-corner -int 5
# defaults write com.apple.dock wvous-br-modifier -int 0

###############################################################################
# "Configuring Safari & WebKit"
###############################################################################

# Set Safari’s home page to ‘about:blank’ for faster loading"
defaults write com.apple.Safari HomePage -string "about:blank"

# Prevent Safari from opening ‘safe’ files automatically after downloading"
defaults write com.apple.Safari AutoOpenSafeDownloads -bool false

# Allow hitting the Backspace key to go to the previous page in history"
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2BackspaceKeyNavigationEnabled -bool true

# Hide Safari’s bookmarks bar by default"
defaults write com.apple.Safari ShowFavoritesBar -bool false

# Hide Safari’s sidebar in Top Sites"
defaults write com.apple.Safari ShowSidebarInTopSites -bool false

# Disable Safari’s thumbnail cache for History and Top Sites"
defaults write com.apple.Safari DebugSnapshotsUpdatePolicy -int 2

# Enable Safari’s debug menu"
defaults write com.apple.Safari IncludeInternalDebugMenu -bool true

# Make Safari’s search banners default to Contains instead of Starts With"
defaults write com.apple.Safari FindOnPageMatchesWordStartsOnly -bool false

# Remove useless icons from Safari’s bookmarks bar"
defaults write com.apple.Safari ProxiesInBookmarksBar "()"
# Enable the Develop menu and the Web Inspector in Safari"
defaults write com.apple.Safari IncludeDevelopMenu -bool true
defaults write com.apple.Safari WebKitDeveloperExtrasEnabledPreferenceKey -bool true
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled -bool true
# Add a context menu item for showing the Web Inspector in web views"
defaults write NSGlobalDomain WebKitDeveloperExtras -bool true

###############################################################################
# Spotlight"
###############################################################################
# # Hide Spotlight tray-icon (and subsequent helper)"
# sudo chmod 600 /System/Library/CoreServices/Search.bundle/Contents/MacOS/Search
# Disable Spotlight indexing for any volume that gets mounted and has not yet been indexed"
# Use `sudo mdutil -i off "/Volumes/foo"` to stop indexing any volume.
# sudo defaults write /.Spotlight-V100/VolumeConfiguration Exclusions -array "/Volumes"
# Change indexing order and disable some file types from being indexed"
defaults write com.apple.spotlight orderedItems -array \
  '{"enabled" = 1;"name" = "APPLICATIONS";}' \
  '{"enabled" = 1;"name" = "SYSTEM_PREFS";}' \
  '{"enabled" = 1;"name" = "DIRECTORIES";}' \
  '{"enabled" = 1;"name" = "PDF";}' \
  '{"enabled" = 1;"name" = "FONTS";}' \
  '{"enabled" = 0;"name" = "DOCUMENTS";}' \
  '{"enabled" = 0;"name" = "MESSAGES";}' \
  '{"enabled" = 0;"name" = "CONTACT";}' \
  '{"enabled" = 0;"name" = "EVENT_TODO";}' \
  '{"enabled" = 0;"name" = "IMAGES";}' \
  '{"enabled" = 0;"name" = "BOOKMARKS";}' \
  '{"enabled" = 0;"name" = "MUSIC";}' \
  '{"enabled" = 0;"name" = "MOVIES";}' \
  '{"enabled" = 0;"name" = "PRESENTATIONS";}' \
  '{"enabled" = 0;"name" = "SPREADSHEETS";}' \
  '{"enabled" = 0;"name" = "SOURCE";}'
# Load new settings before rebuilding the index"
killall mds > /dev/null 2>&1
# Make sure indexing is enabled for the main volume"
sudo mdutil -i on / > /dev/null
## Rebuild the index from scratch"
#sudo mdutil -E / > /dev/null

###############################################################################
# "Terminal & iTerm2"
###############################################################################
# # Only use UTF-8 in Terminal.app"
# defaults write com.apple.terminal StringEncodings -array 4
#
# # Use a modified version of the Solarized Dark theme by default in Terminal.app"
# TERM_PROFILE='Solarized Dark xterm-256color';
# CURRENT_PROFILE="$(defaults read com.apple.terminal 'Default Window Settings')";
# if [ "${CURRENT_PROFILE}" != "${TERM_PROFILE}" ]; then
# 	open "./configs/${TERM_PROFILE}.terminal";
# 	sleep 1; # Wait a bit to make sure the theme is loaded
# 	defaults write com.apple.terminal 'Default Window Settings' -string "${TERM_PROFILE}";
# 	defaults write com.apple.terminal 'Startup Window Settings' -string "${TERM_PROFILE}";
# fi;
## Enable “focus follows mouse” for Terminal.app and all X11 apps"
# i.e. hover over a window and start `typing in it without clicking first
# defaults write com.apple.terminal FocusFollowsMouse -bool true
#defaults write org.x.X11 wm_ffm -bool true
# Installing the Solarized Light theme for iTerm (opening file)"
# open "./configs/Solarized Light.itermcolors"
# Installing the Patched Solarized Dark theme for iTerm (opening file)"
# open "./configs/Solarized Dark Patch.itermcolors"
# Don’t display the annoying prompt when quitting iTerm"
# defaults write com.googlecode.iterm2 PromptOnQuit -bool false
# hide tab title bars"
# defaults write com.googlecode.iterm2 HideTab -bool true
# set system-wide hotkey to show/hide iterm with ^\`"
# defaults write com.googlecode.iterm2 Hotkey -bool true
# hide pane titles in split panes"
# defaults write com.googlecode.iterm2 ShowPaneTitles -bool false
# animate split-terminal dimming"
# defaults write com.googlecode.iterm2 AnimateDimming -bool true
# defaults write com.googlecode.iterm2 HotkeyChar -int 96;
# defaults write com.googlecode.iterm2 HotkeyCode -int 50;
# defaults write com.googlecode.iterm2 FocusFollowsMouse -int 1;
# defaults write com.googlecode.iterm2 HotkeyModifiers -int 262401;
# Make iTerm2 load new tabs in the same directory"
# /usr/libexec/PlistBuddy -c "set \"New Bookmarks\":0:\"Custom Directory\" Recycle" ~/Library/Preferences/com.googlecode.iterm2.plist
# setting fonts"
# defaults write com.googlecode.iterm2 "Normal Font" -string "Hack-Regular 12";
# defaults write com.googlecode.iterm2 "Non Ascii Font" -string "RobotoMonoForPowerline-Regular 12";
# reading iterm settings"
# defaults read -app iTerm > /dev/null 2>&1;
###############################################################################
# "Time Machine"
###############################################################################
# Prevent Time Machine from prompting to use new hard drives as backup volume"
defaults write com.apple.TimeMachine DoNotOfferNewDisksForBackup -bool true
# Disable local Time Machine backups"
hash tmutil &> /dev/null && sudo tmutil disablelocal
###############################################################################
# Activity Monitor
###############################################################################
# Show the main window when launching Activity Monitor"
defaults write com.apple.ActivityMonitor OpenMainWindow -bool true
# Visualize CPU usage in the Activity Monitor Dock icon"
defaults write com.apple.ActivityMonitor IconType -int 5
# Show all processes in Activity Monitor"
defaults write com.apple.ActivityMonitor ShowCategory -int 0
# Sort Activity Monitor results by CPU usage"
defaults write com.apple.ActivityMonitor SortColumn -string "CPUUsage"
defaults write com.apple.ActivityMonitor SortDirection -int 0
###############################################################################
# Address Book, Dashboard, iCal, TextEdit, and Disk Utility
###############################################################################
# Enable the debug menu in Address Book"
defaults write com.apple.addressbook ABShowDebugMenu -bool true
# Enable Dashboard dev mode (allows keeping widgets on the desktop)"
defaults write com.apple.dashboard devmode -bool true
# Use plain text mode for new TextEdit documents"
defaults write com.apple.TextEdit RichText -int 0
# Open and save files as UTF-8 in TextEdit"
defaults write com.apple.TextEdit PlainTextEncoding -int 4
defaults write com.apple.TextEdit PlainTextEncodingForWrite -int 4
# Enable the debug menu in Disk Utility"
defaults write com.apple.DiskUtility DUDebugMenuEnabled -bool true
defaults write com.apple.DiskUtility advanced-image-options -bool true
###############################################################################
# Mac App Store
###############################################################################
# Enable the WebKit Developer Tools in the Mac App Store"
defaults write com.apple.appstore WebKitDeveloperExtras -bool true
# Enable Debug Menu in the Mac App Store"
defaults write com.apple.appstore ShowDebugMenu -bool true
###############################################################################
# Messages
###############################################################################
# Disable automatic emoji substitution (i.e. use plain text smileys)"
# defaults write com.apple.messageshelper.MessageController SOInputLineSettings -dict-add "automaticEmojiSubstitutionEnablediMessage" -bool false
# Disable smart quotes as it’s annoying for messages that contain code"
defaults write com.apple.messageshelper.MessageController SOInputLineSettings -dict-add "automaticQuoteSubstitutionEnabled" -bool false
# Disable continuous spell checking"
defaults write com.apple.messageshelper.MessageController SOInputLineSettings -dict-add "continuousSpellCheckingEnabled" -bool false

# Time to run kristovatlas/osx-config-check
# https://github.com/kristovatlas/osx-config-check
# !!
