-- vim: fdm=marker foldenable
-- FONT DEFS {{{

-- TODO: find a way to check all font sizes to see if they are around the same

local font_presets = {
  ["SourceCodePro"] = {
    name = "Source Code Pro Medium",
    base_size = 15.5,
  },
  ["LiberationMono"] = {
    name = "Liberation Mono",
    base_size = 15,
  },
  ["Terminus"] = {
    name = "Terminus",
    base_size = 18,
  },
  ["Iosevka"] = {
    name = "Iosevka",
    base_size = 16.5,
    supports_ligatures = true, -- FIXME: I don't think so?
  },
  ["FiraCode"] = {
    name = "Fira Code Medium",
    base_size = 15,
  },
  ["CascadiaCode"] = {
    name = "Cascadia Code",
    base_size = 15.5,
  },
  ["JetbrainsMono"] = {
    name = "JetBrains Mono",
    base_size = 15,
  },
  ["Fixedsys"] = {
    name = "Fixedsys Excelsior",
    base_size = 19,
    supports_ligatures = false, -- because of glitches
  },
  ["UbuntuMono"] = {
    name = "Ubuntu Mono",
    base_size = 17,
    supports_ligatures = false,
  },
  ["RobotoMono"] = {
    name = "Roboto Mono Medium",
    base_size = 15,
  },
  ["SpaceMono"] = {
    name = "Space Mono Nerd Font",
    base_size = 15.5,
    supports_ligatures = false,
  },
  ["FantasqueSans"] = {
    name = "Fantasque Sans Mono",
    base_size = 17,
  },
  ["Sudo"] = {
    name = "Sudo",
    base_size = 19.5,
  },
  ["Mononoki"] = {
    name = "Mononoki",
    base_size = 16,
  },
  ["Hack"] = {
    name = "Hack",
    base_size = 14.6,
  },
  ["IbmPlex"] = {
    name = "Ibm Plex Mono",
    base_size = 15,
  },
  ["ShareTech"] = {
    name = "Share Tech Mono",
    base_size = 17,
  },
  ["Unifont"] = {
    name = "Unifont",
    base_size = 18,
  },
  ["ProggyVector"] = {
    name = "ProggyVector",
    base_size = 14.5,
  },
  ["PtMono"] = {
    name = "PTMono",
    base_size = 15,
  },
  ["Hermit"] = {
    name = "Hermit",
    base_size = 14,
  },
  ["Agave"] = {
    name = "Agave",
    base_size = 17,
  },
  ["EnvyCodeR"] = {
    name = "Envy Code R",
    base_size = 16,
  },
  ["DinaRemasterII"] = {
    name = "DinaRemasterII",
    base_size = 18,
  },
  ["Bedstead"] = {
    name = "Bedstead",
    base_size = 16,
  },
  ["ModernDOS"] = {
    name = "Modern DOS 9x16",
    base_size = 17,
  },
  ["GoMono"] = {
    name = "Go Mono",
    base_size = 15,
  },
  ["CourierPrimeCode"] = {
    name = "Courier Prime Code",
    base_size = 17,
  }
}
-- }}}
-- PREPARATIONS {{{
local meta = _G._meta
local theme = meta.theme
local decl = meta.decl

local t_xres = meta.targets.xresources
local t_dots = meta.targets.dotcfg

local _randomFont = {}

local longFontFormat = function(font_name, pixel_size)
  return string.format(
    "%s:pixelsize=%s:antialias=true:autohint=true;",
    font_name,
    pixel_size
  )
end

local getFontInfo = function(name, size_multiplier)
  if name == _randomFont then
    local t = {}
    for k, _ in pairs(font_presets) do
      table.insert(t, k)
    end
    name = t[math.random(1, #t)]
  end

  local val = assert(
    font_presets[name],
    string.format("no such font name: %q", name)
  )

  return {
    name = assert(val.name, "missing font name"),
    base_size = (size_multiplier or 1.0) * assert(val.base_size, "missing base_size"),
    supports_ligatures = (supports_ligatures == nil) and true or supports_ligatures,
  }
end

local fileExists = function(file)
  local fd = io.open(file, "rb")
  if fd then fd:close() end
  return fd ~= nil
end

local trimString = function(str)
  return str:match("^%s*(.-)%s*$")
end

-- Gets the current boot's font.
--
-- If not available, randomly picks one to be such font.
local getBootRandomFont = function()
  local font_path = "/tmp/dotf.random-font.txt"
  if not fileExists(font_path) then
    local keys = {}
    for k, _ in pairs(font_presets) do
      table.insert(keys, k)
    end

    local key = keys[math.random(#keys)]

    local fd = assert(io.open(font_path, "w"), "could not open random font path")
    fd:write(key)
    fd:close()
    return key
  end

  local fd = assert(io.open(font_path, "r"), "could not open random font path")
  local result = trimString(fd:read())
  fd:close()
  return result
end

local T_ALL = {t_xres, t_dots}
-- }}}

local enable_ligatures = false
local font_size = 1.4
local font_name = "SpaceMono"
local font = getFontInfo(font_name, font_size)

local fsize_term = font.base_size
local xft_font = longFontFormat(font.name, fsize_term)

local withAlpha = function(color, alpha)
  return ("%s%02x"):format(color, math.floor(alpha * 255))
end

-- st (x11 terminal)
decl {
  {"st.alpha", "0.75"},
  {"st.cursor", theme["base0D"]},
  {"st.font", xft_font},
  {"st.enableligatures", (enable_ligatures and font.supports_ligatures) and 1 or 0},

  targets = T_ALL,
}

-- foot (wayland terminal)
decl {
  {"foot.font", xft_font},
  {"foot.alpha", "0.95"},

  targets = T_ALL,
}

-- wezterm (terminal)
decl {
  {"wezterm.font", font.name},
  {"wezterm.font_size", font.base_size * font_size * 0.65},

  targets = T_ALL,
}

-- yambar
decl {
  {"yambar.font", longFontFormat(font.name, font.base_size * 0.95)},
  {"yambar.background", theme["base00"]:sub(2) .. "aa"},

  targets = T_ALL,
}

-- waybar
decl {
  {"waybar.font_family", font.name},
  {"waybar.font_size", font.base_size * 0.8},

  targets = T_ALL,
}
-- tym (terminal?)
decl {
  {"tym.font", font.name .. " " .. font.base_size},

  targets = T_ALL,
}

-- urxvt
decl {
  {"uxrvt.font", "xft:" .. xft_font},
  {"urxvt.scrollBar", "false"},
  {"urxvt.keysym.Meta-k", "command:\\033]720;1\\007"},
  {"urxvt.keysym.Meta-j", "command:\\033]721;1\\007"},
  {"urxvt.depth", "32"},
  {"urxvt.background", "[85]" .. theme["base00"]},
  {"urxvt.smoothResize", "true"},

  targets = T_ALL,
}

-- awesomewm
decl {
  {"awesome.font", string.format("%s %spx", font.name, font.base_size * 0.8)},
  {"awesome.border-normal", theme["base00"]},
  {"awesome.border-focus", theme["base03"]},
  {"awesome.border-marked", theme["base0A"]},

  targets = T_ALL,
}

-- hyprland
decl {
  {"hypr.border-normal", theme["base00"]:sub(2)},
  {"hypr.border-focus", theme["base05"]:sub(2)},

  targets = T_ALL,
}

-- riverwm
decl {
  {"river.border-normal", "0x" .. theme["base00"]:sub(2)},
  {"river.border-focus", "0x" .. theme["base05"]:sub(2)},
  {"river.background", "0x" .. theme["base00"]:sub(2)},

  targets = T_ALL,
}

-- dwm
decl {
  {"dwm.norm.bg", theme["base00"]},
  {"dwm.norm.fg", theme["base05"]},
  {"dwm.norm.border", theme["base00"]},
  {"dwm.sel.bg", theme["base02"]},
  {"dwm.sel.fg", theme["base05"]},
  {"dwm.sel.border", theme["base03"]},
  {"dwm.font", xft_font},

  targets = T_ALL,
}

-- bemenu
decl {
  {
    "bemenu.font",
    font.name .. " " .. (font.base_size * 0.75)
  },

  targets = T_ALL,
}

-- dmenu
decl {
  {"dmenu.font", xft_font},
  {"dmenu.norm.bg", theme["base00"]},
  {"dmenu.norm.fg", theme["base05"]},
  {"dmenu.norm.hl.bg", theme["base00"]},
  {"dmenu.norm.hl.fg", theme["base09"]},
  {"dmenu.sel.bg", theme["base02"]},
  {"dmenu.sel.fg", theme["base05"]},
  {"dmenu.sel.hl.bg", theme["base02"]},
  {"dmenu.sel.hl.fg", theme["base09"]},
  {"dmenu.out.bg", theme["base0F"]},
  {"dmenu.out.fg", theme["base0F"]},

  targets = T_ALL,
}

-- xterm
decl {
  {"xterm.font", xft_font},

  targets = T_ALL,
}

-- luakit
decl {
  {"luakit.bg", theme["base00"]},

  targets = T_ALL,
}

-- qtile
decl {
  {"qtile.font-family", font.name},
  {"qtile.font-size", (font.base_size * 0.8)},
  {"qtile.border-focus", theme["base04"]},
  {"qtile.border-normal", theme["base00"]},
  {"qtile.bar.bg", withAlpha(theme["base00"], 0.3)},
  {"qtile.bar.fg", theme["base05"]},
  {"qtile.bar.fg.inactive", theme["base02"]},

  targets = T_ALL,
}

-- qutebrowser
decl {
  {"qutebrowser.font_size", (font_size * 10) .. "pt"},
  {"qutebrowser.fonts.monospace", font.name},
  {"qutebrowser.fonts.standard", font.name},
  {"qutebrowser.fonts.sans-serif", "NotoSansMedium"},
  {"qutebrowser.fonts.serif", font.name},
  {"qutebrowser.bg", theme["base00"]},
  {"qutebrowser.fg", theme["base05"]},
  {"qutebrowser.bg-alt", theme["base01"]},
  {"qutebrowser.bg-alt2", theme["base02"]},
  {"qutebrowser.fg-alt", theme["base04"]},
  {"qutebrowser.fg-alt2", theme["base0A"]},
  {"qutebrowser.bg-attention", theme["base01"]},
  {"qutebrowser.fg-attention", theme["base04"]},
  {"qutebrowser.sel.bg", theme["base02"]},
  {"qutebrowser.sel.fg", theme["base05"]},
  {"qutebrowser.match.fg", theme["base09"]},
  {"qutebrowser.error", theme["base08"]},
  {"qutebrowser.warning", theme["base09"]},
  {"qutebrowser.info", theme["base0D"]},
  {"qutebrowser.success", theme["base0B"]},

  targets = T_ALL,
}

-- others
decl {
  {"Emacs.font", font.name},
  {"pencilwm.highlight", theme["base03"]},
  {"polybar.fontname", font.name},
  {"polybar.fontsize", "9.0"},
  {"gtk3.font", font.name},

  targets = T_ALL,
}

-- general
decl {
  {"*background", theme["base00"]},
  {"*foreground", theme["base05"]},
  {"*bg", theme["base00"]},
  {"*fg", theme["base05"]},
  -- {"*cursor", theme["base0D"]},
  -- {"*cursorColor", theme["base0D"]},

  targets = {t_xres},
}

for i = 0, 15 do
  local hex = string.format("%02X", i)
  local hex_id = "base" .. hex
  local color = theme[hex_id]

  local r = tonumber("0x" .. color:sub(2,3))
  local g = tonumber("0x" .. color:sub(4,5))
  local b = tonumber("0x" .. color:sub(6,7))

  decl {
    {string.format("*.color%02d", i), color},
    {string.format("*.color%d", i), color},
    {"*." .. hex_id, color},

    targets = {t_xres},
  }

  decl {
    {"theme." .. hex_id, color},
    {"theme_no_prefix." .. hex_id, color:sub(2)},
    {"theme_rgb_csv." .. hex_id, r..","..g..","..b},

    targets = T_ALL,
  }
end

return
