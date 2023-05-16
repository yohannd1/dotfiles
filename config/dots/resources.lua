local meta = _G._meta
local theme = meta.theme
local decl = meta.decl

local t_xres = meta.targets.xresources
local t_dots = meta.targets.dotcfg

local function basename(path)
    return path:match("(.*/)")
end

local this_path = debug.getinfo(1, 'S').source:gsub("^@", "")
local this_basename = basename(this_path)

local extra_defs = loadfile(this_basename .. "/" .. "res_extra_defs.lua")()
local longFontFormat = extra_defs.longFontFormat

local chosen_name = "Unifont"
if chosen_name == nil then -- if there's no selected font in the line above, just pick a random one.
    local t = {}
    for name, _ in pairs(extra_defs.font_presets) do
        table.insert(t, name)
    end

    chosen_name = t[math.random(1, #t)]
end

local font_config = assert(extra_defs.font_presets[chosen_name], "no such font name \"" .. chosen_name .. "\"")
local xft_font = longFontFormat(font_config.name, font_config.terminal_pixelsize)

local enable_ligatures = false

local T_ALL = {t_xres, t_dots}

-- st (terminal)
decl {
    {"st.alpha", "0.7"},
    {"st.cursor", theme["base0D"]},
    {"st.font", xft_font},
    {"st.enableligatures", (enable_ligatures and font_config.supports_ligatures) and 1 or 0},

    targets = T_ALL,
}

-- tym (terminal?)
decl {
    {"tym.font", font_config.name .. " " .. font_config.terminal_pixelsize},

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
    {"awesome.font", font_config.name .. " " .. font_config.awesome_size},
    {"awesome.border-normal", theme["base00"]},
    {"awesome.border-focus", theme["base03"]},
    {"awesome.border-marked", theme["base0A"]},

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
    {"qtile.font-family", font_config.name},
    {"qtile.font-size", font_config.terminal_pixelsize},
    {"qtile.border-focus", theme["base03"]},
    {"qtile.border-normal", theme["base00"]},
    {"qtile.bar.bg", theme["base00"]},
    {"qtile.bar.fg", theme["base00"]},
    {"qtile.bar.fg.inactive", theme["base03"]},

    targets = T_ALL,
}

-- qutebrowser
decl {
    {"qutebrowser.font_size", font_config.qutebrowser_size},
    {"qutebrowser.fonts.monospace", font_config.name},
    {"qutebrowser.fonts.standard", font_config.name},
    {"qutebrowser.fonts.sans-serif", "NotoSansMedium"},
    {"qutebrowser.fonts.serif", font_config.name},
    {"qutebrowser.bg", theme["base00"]},
    {"qutebrowser.fg", theme["base05"]},
    {"qutebrowser.bg-alt", theme["base01"]},
    {"qutebrowser.fg-alt", theme["base04"]},
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
    {"Emacs.font", font_config.name},
    {"pencilwm.highlight", theme["base03"]},
    {"polybar.fontname", font_config.name},
    {"polybar.fontsize", "9.0"},
    {"gtk3.font", font_config.name},

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

    decl {
        {string.format("*.color%02d", i), theme[hex_id]},
        {string.format("*.color%d", i), theme[hex_id]},
        {"*." .. hex_id, theme[hex_id]},

        targets = {t_xres},
    }

    decl {
        {"theme." .. hex_id, theme[hex_id]},

        targets = T_ALL,
    }
end

return
