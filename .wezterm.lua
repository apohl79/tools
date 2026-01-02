local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.set_environment_variables = {
  TERMINFO_DIRS = '~/.terminfo',
}

config.term = 'wezterm'

--config.use_ime = false
config.use_dead_keys = false
config.color_scheme = 'Andromeda'
--config.font = wezterm.font 'Iosevka Comfy'
config.font = wezterm.font 'IosevkaTerm NFM Medium'
config.font_size = 13.0
config.window_decorations = "RESIZE"
config.use_fancy_tab_bar = false
config.tab_max_width = 32
config.colors = {
  selection_fg = 'black',
  selection_bg = '#f0faa0',
  tab_bar = {
    background = '#1a1e27',
    active_tab = {
      bg_color = '#262a33',
      fg_color = '#c0c0c0',
    },
    inactive_tab = {
      bg_color = '#1a1e27',
      fg_color = '#808080',
    },
    new_tab = {
      bg_color = '#1a1e27',
      fg_color = '#808080',
    }
  }
}

local act = wezterm.action
config.keys = {
  -- claude shift+enter support
  {
    key = "Enter",
    mods = "SHIFT",
    action = wezterm.action { SendString="\x1b\r" }
  },
  -- tilde hack as it does not work w/o deadkeys enabled
  {
    key = "n",
    mods = "OPT",
    action = act { SendString = "~" },
  },
  -- remove keys I need in emacs
  {
    key = '_',
    mods = 'CTRL|SHIFT',
    action = act.DisableDefaultAssignment,
  },
  {
    key = 'LeftArrow',
    mods = 'CTRL|SHIFT',
    action = act.DisableDefaultAssignment,
  },
  {
    key = 'RightArrow',
    mods = 'CTRL|SHIFT',
    action = act.DisableDefaultAssignment,
  },
  {
    key = 'DownArrow',
    mods = 'CTRL|SHIFT',
    action = act.DisableDefaultAssignment,
  },
  {
    key = 'UpArrow',
    mods = 'CTRL|SHIFT',
    action = act.DisableDefaultAssignment,
  },
  -- split
  {
    key = 'd',
    mods = 'CMD',
    action = act.SplitHorizontal { domain = 'CurrentPaneDomain' },
  },
  {
    key = 'd',
    mods = 'CMD|SHIFT',
    action = act.SplitVertical { domain = 'CurrentPaneDomain' },
  },
  -- pane selection
  {
    key = '9',
    mods = 'CTRL',
    action = act.PaneSelect {
      alphabet = '123456789',
    },
  },
  -- show the pane selection mode, but have it swap the active and selected panes
  {
    key = '0',
    mods = 'CTRL',
    action = act.PaneSelect {
      alphabet = '123456789',
      mode = 'SwapWithActive',
    },
  },
  {
    key = 'LeftArrow',
    mods = 'CMD|META',
    action = act.ActivatePaneDirection 'Left',
  },
  {
    key = 'RightArrow',
    mods = 'CMD|META',
    action = act.ActivatePaneDirection 'Right',
  },
  {
    key = 'UpArrow',
    mods = 'CMD|META',
    action = act.ActivatePaneDirection 'Up',
  },
  {
    key = 'DownArrow',
    mods = 'CMD|META',
    action = act.ActivatePaneDirection 'Down',
  },
  -- resize panes
  {
    key = 'UpArrow',
    mods = 'CMD|SHIFT',
    action = act.AdjustPaneSize { 'Up', 1 },
  },
  {
    key = 'DownArrow',
    mods = 'CMD|SHIFT',
    action = act.AdjustPaneSize { 'Down', 1 },
  },
  {
    key = 'LeftArrow',
    mods = 'CMD|SHIFT',
    action = act.AdjustPaneSize { 'Left', 1 },
  },
  {
    key = 'RightArrow',
    mods = 'CMD|SHIFT',
    action = act.AdjustPaneSize { 'Right', 1 },
  },
  -- tab selection
  {
    key = 'LeftArrow',
    mods = 'CMD',
    action = act.ActivateTabRelative(-1),
  },
  {
    key = 'RightArrow',
    mods = 'CMD',
    action = act.ActivateTabRelative(1),
  },
}

config.mouse_bindings = {
  -- Scrolling up/down while holding CMD adjusts the panel size
  {
    event = { Down = { streak = 1, button = { WheelUp = 1 } } },
    mods = 'CMD',
    action = act.AdjustPaneSize { 'Left', 1 },
  },
  {
    event = { Down = { streak = 1, button = { WheelDown = 1 } } },
    mods = 'CMD',
    action = act.AdjustPaneSize { 'Right', 1 },
  },
}


return config
