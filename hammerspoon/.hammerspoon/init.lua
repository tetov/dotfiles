--[[
Sources:
	functions:
		http://www.hammerspoon.org/go/
	grid:
		https://gist.github.com/moshen/a7c8805fa2827981584d
	hyper key:
		https://github.com/lodestone/hyper-hacks/blob/master/hammerspoon/init.lua
		https://gist.github.com/ttscoff/cce98a711b5476166792d5e6f1ac5907#gistcomment-1886969
--
]]--
-- Set grid size
hs.grid.MARGINX = 0    -- The margin between each window horizontally
hs.grid.MARGINY = 0    -- The margin between each window vertically
hs.grid.GRIDWIDTH = 12  -- The number of cells wide the grid is
hs.grid.GRIDHEIGHT = 12  -- The number of cells high the grid is

-- No animations
hs.window.animationDuration = 0

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

-- A global variable for the Hyper Mode
k = hs.hotkey.modal.new({}, "F17")

launch = function(appname)
  hs.application.launchOrFocus(appname)
  k.triggered = true
end

mvWinLeft = function ()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f)
	k.triggered = true
end

mvWinRight = function ()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x + (max.w / 2)
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f)
	k.triggered = true
end

mvWinTop = function ()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w
  f.h = max.h / 2
  win:setFrame(f)
	k.triggered = true
end

mvWinBottom = function ()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y + (max.h / 2)
  f.w = max.w
  f.h = max.h / 2
  win:setFrame(f)
	k.triggered = true
end

mvWinLeftTwoThirds = function ()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = 2 * max.w / 3
  f.h = max.h
  win:setFrame(f)
	k.triggered = true
end

mvWinLeftOneThird = function ()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w / 3
  f.h = max.h
  win:setFrame(f)
	k.triggered = true
end

mvWinRightTwoThirds = function ()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x + max.w / 3
  f.y = max.y
  f.w = 2 * max.w / 3
  f.h = max.h
  win:setFrame(f)
  
  k.triggered = true
end

mvWinRightOneThird = function ()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x + 2 * max.w / 3
  f.y = max.y
  f.w = max.w / 3
  f.h = max.h
  win:setFrame(f)
  
  k.triggered = true
end

mvWinMiddleOneThird = function ()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + max.w / 3
    f.y = max.y
    f.w = max.w / 3
    f.h = max.h
    win:setFrame(f)

    k.triggered = true
end

mvWinMiddleTwoThirds = function ()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + max.w / 6
    f.y = max.y
    f.w = 2 * max.w / 3
    f.h = max.h
    win:setFrame(f)

    k.triggered = true
end

mvWinLeftTopCorner = function ()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x
    f.y = max.y
    f.w = max.w / 2
    f.h = max.h / 2
    win:setFrame(f)

    k.triggered = true
end
  
mvWinRightTopCorner = function ()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + max.w / 2
    f.y = max.y
    f.w = max.w / 2
    f.h = max.h / 2
    win:setFrame(f)

    k.triggered = true
end

mvWinLeftBottomCorner = function ()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x
    f.y = max.y + max.h / 2
    f.w = max.w / 2
    f.h = max.h / 2
    win:setFrame(f)

    k.triggered = true
end

mvWinRightBottomCorner = function ()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + max.w / 2
    f.y = max.y + max.h / 2
    f.w = max.w / 2
    f.h = max.h / 2
    win:setFrame(f)

    k.triggered = true
end

maxset = function()
	hs.grid.maximizeWindow(hs.window.focusedWindow())
	k.triggered = true
end

-- Shift window on grid
shiftLeft = function ()
	hs.grid.pushWindowLeft(hs.window.focusedWindow())
	k.triggered = true
end

shiftDown = function ()
	hs.grid.pushWindowDown(hs.window.focusedWindow())
	k.triggered = true
end

shiftUp = function ()
	hs.grid.pushWindowUp(hs.window.focusedWindow())
	k.triggered = true
end

shiftRight = function ()
	hs.grid.pushWindowRight(hs.window.focusedWindow())
	k.triggered = true
end

-- Resize window on grid
taller = function ()
    hs.grid.resizeWindowTaller(hs.window.focusedWindow())
    k.triggered = true
end

shorter = function ()
    hs.grid.resizeWindowShorter(hs.window.focusedWindow())
    k.triggered = true
end

wider = function ()
    hs.grid.resizeWindowWider(hs.window.focusedWindow())
    k.triggered = true
end

thinner = function ()
    hs.grid.resizeWindowThinner(hs.window.focusedWindow())
    k.triggered = true
end

-- Enter Hyper Mode when F18 (Hyper/Capslock) is pressed
pressedF18 = function()
  k.triggered = false
  k:enter()
end

-- Leave Hyper Mode when F18 (Hyper/Capslock) is pressed,
--   send ESCAPE if no other keys are pressed.
releasedF18 = function()
  k:exit()
  if not k.triggered then
    hs.eventtap.keyStroke({}, 'ESCAPE')
  end
end

-- Bind the Hyper key
f18 = hs.hotkey.bind({}, 'F18', pressedF18, releasedF18)

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

-- Paste fungerar trots blockering (t ex mount .dmg i finder)
hs.hotkey.bind({"cmd", "alt"}, "V", function() hs.eventtap.keyStrokes(hs.pasteboard.getContents()) end)

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

-- Keybindings

k:bind({}, 'left', nil, mvWinLeft)
k:bind({}, 'right', nil, mvWinRight)

k:bind({}, 'up', nil, mvWinTop)
k:bind({}, 'down', nil, mvWinBottom)

k:bind({}, '1', nil, mvWinLeftOneThird)
k:bind({}, '2', nil, mvWinMiddleOneThird)
k:bind({}, '3', nil, mvWinRightOneThird)

k:bind({}, '4', nil, mvWinLeftTwoThirds)
k:bind({}, '5', nil, mvWinMiddleTwoThirds)
k:bind({}, '6', nil, mvWinRightTwoThirds)

k:bind({}, '7', nil, mvWinLeftTopCorner)
k:bind({}, '8', nil, mvWinRightTopCorner)
k:bind({}, '9', nil, mvWinLeftBottomCorner)
k:bind({}, '0', nil, mvWinRightBottomCorner)

k:bind({}, 'm', nil, maxset)

k:bind({}, "h", nil, shiftLeft)
k:bind({}, "j", nil, shiftUp)
k:bind({}, "k", nil, shiftDown)
k:bind({}, "l", nil, shiftRight)

k:bind({}, "y", nil, wider)
k:bind({}, "u", nil, taller)
k:bind({}, "i", nil, shorter)
k:bind({}, "o", nil, thinner)

-- Single keybinding for app launch
singleapps = {
-- First row
  {'q', 'iTerm'},
  {'w', 'Google Chrome'},
  {'e', 'Finder'},
  {'r', 'Remember the milk'},
-- Second row
  {'a', '/Applications/Atom.app'}, -- Full path because otherwise launch duplicate Atom apps
  {'s', 'Standard Notes'},
  {'d', 'System Preferences'},
-- Third row
  {'z', 'Messages'},
  {'x', 'Messenger'},
  {'c', 'Calendar'}
}

for i, app in ipairs(singleapps) do
  k:bind({}, app[1], function() launch(app[2]); k:exit(); end)
end
