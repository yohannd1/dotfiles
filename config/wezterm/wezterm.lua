local wezterm = require("wezterm")
local config = wezterm.config_builder()
local act = wezterm.action

local getRes = function(res_list)
  local get_args = {}
  for _, pair in ipairs(res_list) do
    table.insert(get_args, "get:" .. pair[2])
  end

  local cmd = ("dotcfg send $'%s'"):format(table.concat(get_args, "\\n"))

  local proc = io.popen(cmd)
  local iter = proc:lines()
  local result = {}

  for _, pair in ipairs(res_list) do
    local name = pair[1]
    result[name] = iter()
  end

  return result
end

local res = getRes({
  {"alpha", "st.alpha"},
  {"ligatures", "st.enableligatures"},
  {"font", "wezterm.font"},
  {"font_size", "wezterm.font_size"},
  {"base00", "theme.base00"},
  {"base01", "theme.base01"},
  {"base02", "theme.base02"},
  {"base03", "theme.base03"},
  {"base04", "theme.base04"},
  {"base05", "theme.base05"},
  {"base06", "theme.base06"},
  {"base07", "theme.base07"},
  {"base08", "theme.base08"},
  {"base09", "theme.base09"},
  {"base0A", "theme.base0A"},
  {"base0B", "theme.base0B"},
  {"base0C", "theme.base0C"},
  {"base0D", "theme.base0D"},
  {"base0E", "theme.base0E"},
  {"base0F", "theme.base0F"},
})

config.enable_tab_bar = false
config.window_background_opacity = tonumber(res.alpha)

config.font = wezterm.font(res.font)
config.font_size = tonumber(res.font_size)

config.colors = {
  foreground = res.base06,
  background = res.base00,

  cursor_bg = '#52ad70',
  cursor_fg = "#000000",

  selection_fg = 'black',
  selection_bg = '#fffacd',

  ansi = {
    res.base00,
    res.base01,
    res.base02,
    res.base03,
    res.base04,
    res.base05,
    res.base06,
    res.base07,
  },
  brights = {
    res.base08,
    res.base09,
    res.base0A,
    res.base0B,
    res.base0C,
    res.base0D,
    res.base0E,
    res.base0F,
  },
}

local enable_ligatures = res.ligatures == "0"
config.harfbuzz_features = { "calt=1", "clig=1", enableligatures and "liga=1" or "liga=0" }

config.disable_default_key_bindings = true
config.keys = {
  -- scroll
  { key = "j", mods = "ALT|SHIFT", action = act.IncreaseFontSize },
  { key = "k", mods = "ALT|SHIFT", action = act.DecreaseFontSize },

  -- zoom
  { key = "j", mods = "ALT|SHIFT", action = act.IncreaseFontSize },
  { key = "k", mods = "ALT|SHIFT", action = act.DecreaseFontSize },

  -- clipboard
  { key = "c", mods = "ALT", action = act.CopyTo("Clipboard") },
  { key = "v", mods = "ALT", action = act.PasteFrom("Clipboard") },
}

return config
