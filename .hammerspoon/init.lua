log = hs.logger.new("init", 3)

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
hs.hotkey.bind({"ctrl", "cmd"}, "P", function()
    local output, status = hs.execute("op item get 'parloa okta' --fields username,password --reveal", true)
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
