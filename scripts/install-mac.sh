#!/usr/bin/env sh

# Script for new installation on mac
# Do not execute, run line by line.
#
# Sources
# https://github.com/drduh/macOS-Security-and-Privacy-Guide
# https://github.com/atomantic/dotfiles/blob/master/install.sh

# Brew stuff
## Install homebrew (check command before running)
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

brew update
brew upgrade

brew tap Homebrew/bundle
brew bundle # see ./Brewfile

. "$HOME/tetov-dotfiles/stow.sh"
. "$HOME/tetov-dotfiles/install-common.sh"

## Set zsh as the user login shell
chsh -s "$(which zsh)"

brew cleanup

mackup restore # after logged in to Dropbox

# Start drduh/macOS-Security-and-Privacy-Guide recommendations
## Use admin account

## Firmware password

### "The firmware password can also be managed with the firmwarepasswd utility while booted into the OS. For example, to prompt for the firmware password when attempting to boot from a different volume:""
sudo firmwarepasswd -setpasswd -setmode command

### Enable firewall
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setglobalstate on

### Enable logging
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setloggingmode on

## Host file
### Append and edit /etc/hosts from:
### http://someonewhocares.org/hosts/zero/hosts
### https://github.com/l1k/osxparanoia/blob/master/hosts
### https://github.com/StevenBlack/hosts
### https://github.com/gorhill/uMatrix/blob/master/assets/umatrix/hosts-files.json

### To append:
### curl "URL" | sudo tee -a /etc/hosts

### To check length:
### wc -l /etc/hosts

### To test
### egrep -ve "^#|^255.255.255|^0.0.0.0|^127.0.0.0|^0 " /etc/hosts

### Expected output:
### ::1 localhost
### fe80::1%lo0 localhost

## Computer name, host name

sudo scutil --set ComputerName your_computer_name
sudo scutil --set LocalHostName your_hostname

## Disable captive portal function

sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.captive.control Active -bool false

## Setup privoxy

### Start service
sudo brew services start privoxy

### Set http-proxy
sudo networksetup -setwebproxy "Wi-Fi" 127.0.0.1 8118

### Set https-proxy
sudo networksetup -setsecurewebproxy "Wi-Fi" 127.0.0.1 8118

### Confirm proxy set
scutil --proxy

#### Expected output:
#### <dictionary> {
####  ExceptionsList : <array> {
####    0 : *.local
####    1 : 169.254/16
####  }
####  FTPPassive : 1
####  HTTPEnable : 1
####  HTTPPort : 8118
####  HTTPProxy : 127.0.0.1
#### }

### and
ALL_PROXY=127.0.0.1:8118 curl -I http://p.p/

#### Expected output:
#### HTTP/1.1 200 OK
#### Content-Length: 2401
#### Content-Type: text/html
#### Cache-Control: no-cache

### Edit user.action and config according to backup gist
### https://gist.github.com/sonyamamurin/8b91a9b18452a5692b3ad0e39b06dd86

## Setup spoof-mac

### Edit homebrew.mxcl.spoof-mac.plist in /usr/local/Cellar/spoof-mac/x.x.x
### to look like this: (make en1 is the right one, run spoof-mac list)

#### <?xml version="1.0" encoding="UTF-8"?>
####<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
####<plist version="1.0">
####  <dict>
####    <key>Label</key>
####    <string>homebrew.mxcl.spoof-mac</string>
####    <key>ProgramArguments</key>
####    <array>
####      <string>/usr/local/opt/spoof-mac/bin/spoof-mac</string>
####      <string>randomize</string>
####      <string>en1</string>
####    </array>
####    <key>RunAtLoad</key>
####    <true/>
####    <key>StandardErrorPath</key>
####    <string>/dev/null</string>
####    <key>StandardOutPath</key>
####    <string>/dev/null</string>
####  </dict>
####</plist>

# Start of commands from atomantic/dotfiles
## https://github.com/atomantic/dotfiles/blob/master/install.sh

## Disable guest account login
sudo defaults write /Library/Preferences/com.apple.loginwindow GuestEnabled -bool false

## SSD specific tweaks
### Disable local Time Machine snapshots
sudo tmutil disablelocal

### Disable hibernation (speeds up entering sleep mode)"
sudo pmset -a hibernatemode 0

### Remove the sleep image file to save disk space
sudo rm -rf /Private/var/vm/sleepimage
### Create a zero-byte file instead
sudo touch /Private/var/vm/sleepimage
### …and make sure it can’t be rewritten
sudo chflags uchg /Private/var/vm/sleepimage

## Menu bar

### Disable transparency
defaults write NSGlobalDomain AppleEnableMenuBarTransparency -bool false

### Menu bar: hide the Time Machine, Volume, User, and Bluetooth icons"
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


## Expand save panel by default
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true

## Expand print panel by default
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true

## Save to disk (not to iCloud) by default
defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false

## Automatically quit printer app once the print jobs complete"
defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true

## Disable the crash reporter"
defaults write com.apple.CrashReporter DialogType -string "none"

## Set Help Viewer windows to non-floating mode"
defaults write com.apple.helpviewer DevMode -bool true

## Restart automatically if the computer freezes"
sudo systemsetup -setrestartfreeze on

## Check for software updates daily, not just once per week"
defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1

## Disable smart quotes as they’re annoying when typing code"
defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false

## Disable smart dashes as they’re annoying when typing code"
defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false

## Increase sound quality for Bluetooth headphones/headsets"
defaults write com.apple.BluetoothAudioAgent "Apple Bitpool Min (editable)" -int 40

## Enable full keyboard access for all controls (e.g. enable Tab in modal dialogs)"
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3

## Disable press-and-hold for keys in favor of key repeat"
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

## Set a blazingly fast keyboard repeat rate"
defaults write NSGlobalDomain KeyRepeat -int 1
defaults write NSGlobalDomain InitialKeyRepeat -int 10

## Disable auto-correct"
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false

## Require password immediately after sleep or screen saver begins"
defaults write com.apple.screensaver askForPassword -int 1
defaults write com.apple.screensaver askForPasswordDelay -int 0

## Save screenshots to the desktop"
defaults write com.apple.screencapture location -string "${HOME}/Desktop"

## Save screenshots in PNG format (other options: BMP, GIF, JPG, PDF, TIFF)"
defaults write com.apple.screencapture type -string "png"

## Disable shadow in screenshots"
defaults write com.apple.screencapture disable-shadow -bool true

## Enable subpixel font rendering on non-Apple LCDs"
defaults write NSGlobalDomain AppleFontSmoothing -int 2

## Finder

### Allow quitting Finder via ⌘ + Q; doing so will also hide desktop icons"
defaults write com.apple.finder QuitMenuItem -bool true

### Set ~ as the default location for new Finder windows"
### For other paths, use 'PfLo' and 'file:///full/path/here/'
defaults write com.apple.finder NewWindowTarget -string "PfHm"

### Show hidden files by default
defaults write com.apple.finder AppleShowAllFiles -bool true

### Show all filename extensions
defaults write NSGlobalDomain AppleShowAllExtensions -bool true

### Show status bar
defaults write com.apple.finder ShowStatusBar -bool true

### Show path bar
defaults write com.apple.finder ShowPathbar -bool true

###  Allow text selection in Quick Look
defaults write com.apple.finder QLEnableTextSelection -bool true

### Display full POSIX path as Finder window title
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

###  When performing a search, search the current folder by default
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

###  Disable the warning when changing a file extension
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

###  Enable spring loading for directories
defaults write NSGlobalDomain com.apple.springing.enabled -bool true

###  Remove the spring loading delay for directories
defaults write NSGlobalDomain com.apple.springing.delay -float 0

###  Avoid creating .DS_Store files on network volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

###  Use list view in all Finder windows by default
### Four-letter codes for the other view modes: `icnv`, `clmv`, `Flwv`
defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"

###  Empty Trash securely by default
defaults write com.apple.finder EmptyTrashSecurely -bool true

###  Show the ~/Library folder
chflags nohidden ~/Library

###  Expand the following File Info panes: “General”, “Open with”, and “Sharing & Permissions”"
defaults write com.apple.finder FXInfoPanesExpanded -dict \
  General -bool true \
  OpenWith -bool true \
  Privileges -bool true

## "Dock & Dashboard"

###  Enable highlight hover effect for the grid view of a stack (Dock)"
defaults write com.apple.dock mouse-over-hilite-stack -bool true

###  Enable spring loading for all Dock items
defaults write com.apple.dock enable-spring-load-actions-on-all-items -bool true

###  Show indicator lights for open applications in the Dock"
defaults write com.apple.dock show-process-indicators -bool true

###  Speed up Mission Control animations
defaults write com.apple.dock expose-animation-duration -float 0.1

### Don’t group windows by application in Mission Control
### (i.e. use the old Exposé behavior instead)
defaults write com.apple.dock expose-group-by-app -bool false

### Disable Dashboard
defaults write com.apple.dashboard mcx-disabled -bool true

### Don’t show Dashboard as a Space
defaults write com.apple.dock dashboard-in-overlay -bool true

### Don’t automatically rearrange Spaces based on most recent use
defaults write com.apple.dock mru-spaces -bool false

### Remove the auto-hiding Dock delay
defaults write com.apple.dock autohide-delay -float 0
###  Remove the animation when hiding/showing the Dock"
defaults write com.apple.dock autohide-time-modifier -float 0

### Automatically hide and show the Dock
defaults write com.apple.dock autohide -bool true

### Make Dock icons of hidden applications translucent
defaults write com.apple.dock showhidden -bool true

### Make Dock more transparent
defaults write com.apple.dock hide-mirror -bool true

## Configuring Safari & WebKit

### Set Safari’s home page to ‘about:blank’ for faster loading"
defaults write com.apple.Safari HomePage -string "about:blank"

### Prevent Safari from opening ‘safe’ files automatically after downloading"
defaults write com.apple.Safari AutoOpenSafeDownloads -bool false

### Allow hitting the Backspace key to go to the previous page in history"
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2BackspaceKeyNavigationEnabled -bool true

### Enable the Develop menu and the Web Inspector in Safari"
defaults write com.apple.Safari IncludeDevelopMenu -bool true
defaults write com.apple.Safari WebKitDeveloperExtrasEnabledPreferenceKey -bool true
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled -bool true

## Terminal

### Only use UTF-8 in Terminal.app"
defaults write com.apple.terminal StringEncodings -array 4

## Time Machine

### Prevent Time Machine from prompting to use new hard drives as backup volume"
defaults write com.apple.TimeMachine DoNotOfferNewDisksForBackup -bool true

### Disable local Time Machine backups"
hash tmutil >/dev/null 2>&1 && sudo tmutil disablelocal

## Activity Monitor

### Show the main window when launching Activity Monitor"
defaults write com.apple.ActivityMonitor OpenMainWindow -bool true

### Visualize CPU usage in the Activity Monitor Dock icon"
defaults write com.apple.ActivityMonitor IconType -int 5

### Show all processes in Activity Monitor"
defaults write com.apple.ActivityMonitor ShowCategory -int 0

## Address Book, Dashboard, iCal, TextEdit, and Disk Utility

### Enable the debug menu in Address Book"
defaults write com.apple.addressbook ABShowDebugMenu -bool true

### Enable Dashboard dev mode (allows keeping widgets on the desktop)"
defaults write com.apple.dashboard devmode -bool true

### Use plain text mode for new TextEdit documents"
defaults write com.apple.TextEdit RichText -int 0

### Open and save files as UTF-8 in TextEdit"
defaults write com.apple.TextEdit PlainTextEncoding -int 4
defaults write com.apple.TextEdit PlainTextEncodingForWrite -int 4

### Enable the debug menu in Disk Utility"
defaults write com.apple.DiskUtility DUDebugMenuEnabled -bool true
defaults write com.apple.DiskUtility advanced-image-options -bool true

## Mac App Store

### Enable the WebKit Developer Tools in the Mac App Store"
defaults write com.apple.appstore WebKitDeveloperExtras -bool true

### Enable Debug Menu in the Mac App Store"
defaults write com.apple.appstore ShowDebugMenu -bool true

## Messages

###  Disable smart quotes as it’s annoying for messages that contain code"
defaults write com.apple.messageshelper.MessageController SOInputLineSettings -dict-add "automaticQuoteSubstitutionEnabled" -bool false

### Disable continuous spell checking"
defaults write com.apple.messageshelper.MessageController SOInputLineSettings -dict-add "continuousSpellCheckingEnabled" -bool false
