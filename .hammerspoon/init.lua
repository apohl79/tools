log = hs.logger.new("init", 3)

-- Enable IPC so the hs CLI (`hs -c '...'`) can query state from outside.
pcall(function() require('hs.ipc') end)

-- Create a meet room and add the link below to paste on CTRL+M
hs.hotkey.bind({"ctrl"}, "M", function()
    hs.eventtap.keyStrokes("meet.google.com/aur-dvoi-hgr")
    --hs.eventtap.keyStroke({}, "return")
end)

-- hs.hotkey.bind({"option"}, "Q", function()
--     hs.eventtap.keyStroke({"cmd"}, "q")
-- end)
--
-- hs.hotkey.bind({"option"}, "delete", function()
--     hs.eventtap.keyStroke({}, "escape")
-- end)
--
-- function isIpad()
--   rect = hs.screen.mainScreen():fullFrame()
--   if ((rect.w == 1440 and rect.h == 986) or (rect.w == 1680 and rect.h == 1150)) then
--     return true
--   end
--   return false
-- end
--
-- -- Splashtop is not forwarding keys in a way that macOS app receive them the same way as the HW
-- -- keyboad generates them. So we use a little hack, as cmd+v works, we just insert the key into
-- -- the clipboard and paste.
-- function ipadHelper(key)
--     if (isIpad()) then
--       hs.pasteboard.setContents(key)
--       hs.eventtap.keyStroke({"cmd"}, "v")
--     else
--       hs.eventtap.keyStrokes(key)
--     end
-- end
--
-- hs.hotkey.bind({"option"}, "5", function() ipadHelper("[") end)
-- hs.hotkey.bind({"option"}, "6", function() ipadHelper("]") end)
-- hs.hotkey.bind({"option"}, "7", function() ipadHelper("|") end)
-- hs.hotkey.bind({"option"}, "8", function() ipadHelper("{") end)
-- hs.hotkey.bind({"option"}, "9", function() ipadHelper("}") end)

-- -- Toggle play/pause in spotify hs.hotkey.bind({}, "F13", function()
-- hs.spotify.playpause() end)
--
-- Parloa Okta login helper
-- Username and password
hs.hotkey.bind({"ctrl", "cmd"}, "P", function()
    local output, status = hs.execute("op item --account my.1password.eu get 'parloa okta' --fields username,password --reveal", true)
    if status then
        output = output:gsub("%s+$", "")
        local username, password = output:match("([^,]+),([^,]+)")
        hs.eventtap.keyStrokes(username)
        hs.eventtap.keyStroke({}, "tab")
        hs.eventtap.keyStrokes(password)
    else
        hs.alert.show("Failed to retrieve credentials from 1Password")
    end
end)

-- Password
hs.hotkey.bind({"ctrl", "option", "cmd"}, "P", function()
    local output, status = hs.execute("op item --account my.1password.eu get 'parloa okta' --fields username,password --reveal", true)
    if status then
        output = output:gsub("%s+$", "")
        local username, password = output:match("([^,]+),([^,]+)")
        hs.eventtap.keyStrokes(password)
    else
        hs.alert.show("Failed to retrieve credentials from 1Password")
    end
end)

-- Username
hs.hotkey.bind({"option", "cmd"}, "P", function()
    local output, status = hs.execute("op item --account my.1password.eu get 'parloa email' --fields username --reveal", true)
    if status then
        output = output:gsub("%s+$", "")
        hs.eventtap.keyStrokes(output)
    else
        hs.alert.show("Failed to retrieve credentials from 1Password")
    end
end)

-- ===========================================================
-- cmux right-Option workaround for the German keyboard layout.
--
-- cmux currently intercepts BOTH Option keys as Alt/Meta even
-- with `macos-option-as-alt = left`, so right-Option specials
-- (~ @ | [ ] { } € µ) never reach the terminal. Tracked in:
--   https://github.com/manaflow-ai/cmux/issues/1397
--   https://github.com/manaflow-ai/cmux/pull/2371  (fix, unmerged)
--
-- Stopgap: when cmux is frontmost AND only right-Option is held
-- (no left-Option / Cmd / Ctrl), inject the German layout's
-- right-Option result via keyStrokes and swallow the original
-- key event so cmux never sees the bogus Alt+key.
-- ===========================================================
local cmuxRightOpt = { rightDown = false, leftDown = false }

local cmuxRightOptMap = {
  -- char (modifier-stripped, lowercased) -> { plain, with_shift }
  ["n"] = { "~", "~"  },
  ["l"] = { "@", nil  },
  ["7"] = { "|", "\\" },
  ["5"] = { "[", nil  },
  ["6"] = { "]", nil  },
  ["8"] = { "{", nil  },
  ["9"] = { "}", nil  },
  ["e"] = { "€", nil  },
  ["m"] = { "µ", nil  },
}

local CMUX_BUNDLE = "com.cmuxterm.app"

cmuxRightOptFlagsTap = hs.eventtap.new(
  { hs.eventtap.event.types.flagsChanged },
  function(e)
    local kc = e:getKeyCode()
    local altActive = e:getFlags().alt == true
    if kc == 61 then       -- right option keycode
      cmuxRightOpt.rightDown = altActive
    elseif kc == 58 then   -- left option keycode
      cmuxRightOpt.leftDown  = altActive
    end
    return false
  end
)
cmuxRightOptFlagsTap:start()

cmuxRightOptKeyTap = hs.eventtap.new(
  { hs.eventtap.event.types.keyDown },
  function(e)
    if not cmuxRightOpt.rightDown then return false end
    if cmuxRightOpt.leftDown then return false end
    local f = e:getFlags()
    if f.cmd or f.ctrl or f.fn then return false end

    local app = hs.application.frontmostApplication()
    if not app or app:bundleID() ~= CMUX_BUNDLE then return false end

    local clean = (e:getCharacters(true) or ""):lower()
    local mapping = cmuxRightOptMap[clean]
    if not mapping then return false end

    local out = f.shift and mapping[2] or mapping[1]
    if not out then return false end

    hs.eventtap.keyStrokes(out)
    return true  -- swallow the original event
  end
)
cmuxRightOptKeyTap:start()

-- ===========================================================
-- cmux browser-zoom-in workaround for the German keyboard.
--
-- cmux's shortcut config can only bind by produced character.
-- The `plus` alias normalizes to `=` (US layout), so on German
-- (where the dedicated `+` key produces `+`, not `=`),
-- `cmd+=` never matches `cmd+`+`` and browserZoomIn is unreachable
-- from that key. cmd+`-` (zoom out) and cmd+`0` (reset) work
-- natively because their characters match.
--
-- Stopgap: when cmux is frontmost and `cmd+`+`` is pressed,
-- swallow it and synthesize `cmd+shift+0`. On German that
-- produces `=` (shift+0 = `=`), which cmux's `cmd+=` binding
-- then matches → fires browserZoomIn. Restart the workaround
-- when cmux fixes shortcut keycode handling.
-- ===========================================================
-- cmux's shortcut matcher can't bind the literal `+` character (parser
-- splits on `+` and rejects empty tokens) AND seems to ignore Hammer-
-- spoon-synthesized events. Bypass both by invoking the cmux View
-- menu item directly via AppKit when cmd+`+` (German `+` key) is
-- pressed inside cmux. cmd+`-` and cmd+`0` already work natively.
cmuxBrowserZoomInTap = hs.eventtap.new(
  { hs.eventtap.event.types.keyDown },
  function(e)
    local app = hs.application.frontmostApplication()
    if not app or app:bundleID() ~= CMUX_BUNDLE then return false end
    if cmuxRightOpt.rightDown then return false end

    local f = e:getFlags()
    if not f.cmd or f.ctrl or f.fn or f.shift then return false end

    local clean = e:getCharacters(true) or ""
    if clean ~= "+" then return false end

    local cmuxApp = hs.application.applicationsForBundleID(CMUX_BUNDLE)[1]
    if cmuxApp then
      cmuxApp:selectMenuItem({ "View", "Zoom In" }, true)
    end
    return true  -- swallow original cmd+`+'
  end
)
cmuxBrowserZoomInTap:start()

-- hs.hotkey.bind({}, "F14", function()
--       hs.osascript.applescript([[
-- tell application "Google Chrome"
--      repeat with cur_win in every window
--             set i to 0
--             repeat with cur_tab in every tab of cur_win
--                    set i to i + 1
--                    if (URL of cur_tab starts with "https://play.google.com") then
--                       # found play music tab, now get it to the front
--                       set (active tab index of (cur_win)) to i
--                       set index of cur_win to 1
--                       tell application "System Events" to tell process "Google Chrome"
--                            perform action "AXRaise" of window 1
--                       end tell
--                       activate
--                       delay 0.5
--                       # try to toggle play/pause
--                       tell application "System Events" to keystroke space
--                       return "Toggle play/pause on tab " & URL of cur_tab
--                    end if
--             end repeat
--      end repeat
--      return "No play music tab found"
-- end tell
-- ]])
-- end)
