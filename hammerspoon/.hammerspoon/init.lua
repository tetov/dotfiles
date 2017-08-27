-- ENV

-- No animations
hs.window.animationDuration = 0

-- capture before binding with hyper
hyperfns = {}

-- FUNCTIONS

-- Reload config when .lua files are modified
function reloadConfig(files)
  doReload = false
  for _,file in pairs(files) do
    if file:sub(-4) == ".lua" then
      doReload = true
    end
  end
  if doReload then
    hs.reload()
  end
end

local myWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()

hs.alert.show("Config loaded")

-- Move window to part of screen
leftHalf = { 0, 0, 0.5, 1 }
rightHalf = { 0.5, 0, 0.5, 1 }
leftThird = { 0, 0, 1/3, 1 }
rightThird = { 2/3, 0, 1/3, 1 }
leftTwoThirds = { 0, 0, 2/3, 1 }
rightTwoThirds = { 1/3, 0, 2/3, 1 }


function moveWindow(unitCoord)
    hs.window.focusedWindow():moveToUnit(hs.geometry.rect(unitCoord))
end

-- Get and move to screen
function getNextScreen(s)
    all = hs.screen.allScreens()
    for i = 1, #all do
        if all[i] == s then
            return all[(i - 1 + 1) % #all + 1]
        end
    end
    return nil
end

function moveToNextScreen()
    local win = hs.window.focusedWindow()
    if win ~= nil then
        currentScreen = win:screen()
        nextScreen = getNextScreen(currentScreen)
        if nextScreen then
            win:moveToScreen(nextScreen)
        end
    end
end

-- Toggle a window between its normal size, and being maximized
local frameCache = {}

function toggleMaximized()
  local win = hs.window.focusedWindow()
  if frameCache[win:id()] then
    win:setFrame(frameCache[win:id()])
    frameCache[win:id()] = nil
  else
    frameCache[win:id()] = win:frame()
    win:maximize()
  end
end

-- Bring all Finder windows forward when one gets activated
function applicationWatcher(appName, eventType, appObject)
  if (eventType == hs.application.watcher.activated) then
    if (appName == "Finder") then
      appObject:selectMenuItem({"Window", "Bring All to Front"})
    end
  end
end
local appWatcher = hs.application.watcher.new(applicationWatcher)
appWatcher:start()


-- Ändra volym baserat på wifi-nätverk
local wifiWatcher = nil
local homeSSID1 = "Mullberget"
local homeSSID2 = "Mullberget2"
local lastSSID = hs.wifi.currentNetwork()

function ssidChangedCallback()
  newSSID = hs.wifi.currentNetwork()

	atHome = (newSSID == homeSSID1 or newSSID == homeSSID2)
	prevAtHome = (lastSSID == homeSSID1 or lastSSID == homeSSID2)

  if atHome == true and prevAtHome == false then
    -- We just joined our home WiFi network
    hs.audiodevice.defaultOutputDevice():setVolume(25)
		hs.printf("Just came home")
    elseif atHome == false and prevAtHome == true then
      -- We just departed our home WiFi network
      hs.audiodevice.defaultOutputDevice():setVolume(0)
		hs.printf("Just left home")
    end

    lastSSID = newSSID
end
wifiWatcher = hs.wifi.watcher.new(ssidChangedCallback)
wifiWatcher:start()

-- KEYBINDINGS

-- Spotlight

-- A global variable for the Hyper Mode
hyper = {"⌘", "⌥", "⌃", "⇧"}

-- § to Spotlight
hs.hotkey.bind("", "§", function() hs.eventtap.keyStroke({"⌘"}, 'space') end)

-- Move to next screen
hyperfns['<'] = moveToNextScreen

-- Window Hints
hyperfns['+'] = hs.hints.windowHints

-- Get past pasteblocking
hyperfns['v'] = function() hs.eventtap.keyStrokes(hs.pasteboard.getContents()) end

-- Window layouts
hyperfns['1'] = function() moveWindow(leftHalf) end
hyperfns['2'] = function() moveWindow(rightHalf) end
hyperfns['3'] = function() moveWindow(leftThird) end
hyperfns['4'] = function() moveWindow(rightThird) end
hyperfns['5'] = function() moveWindow(leftTwoThirds) end
hyperfns['6'] = function() moveWindow(rightTwoThirds) end

hyperfns['m'] = toggleMaximized

-- Change focus
hyperfns['h'] = function() hs.window.focusedWindow():focusWindowWest() end
hyperfns['l'] = function() hs.window.focusedWindow():focusWindowEast() end
hyperfns['k'] = function() hs.window.focusedWindow():focusWindowNorth() end
hyperfns['j'] = function() hs.window.focusedWindow():focusWindowSouth() end

-- Launch apps
hyperfns['q'] = function() hs.application.launchOrFocus('iTerm') end
hyperfns['w'] = function() hs.application.launchOrFocus('Google Chrome') end
hyperfns['e'] = function() hs.application.launchOrFocus('Finder') end
hyperfns['r'] = function() hs.application.launchOrFocus('Remember the milk') end

hyperfns['a'] = function() hs.urlevent.openURLWithBundle('https://mail.google.com/', 'com.google.chrome') end
hyperfns['s'] = function() hs.application.launchOrFocus('Standard Notes') end
hyperfns['d'] = function() hs.application.launchOrFocus('System Preferences') end

hyperfns['z'] = function() hs.application.launchOrFocus('Messages') end
hyperfns['x'] = function() hs.application.launchOrFocus('Caprine') end
hyperfns['c'] = function() hs.urlevent.openURLWithBundle('https://calendar.google.com/', 'com.google.chrome') end

-- Set up hyper keybindings
for _hotkey, _fn in pairs(hyperfns) do
    hs.hotkey.bind(hyper, _hotkey, _fn)
end
