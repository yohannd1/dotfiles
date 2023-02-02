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

local chosen_name = "Mononoki"
if chosen_name == nil then -- if there's no selected font in the line above, just pick a random one.
    local t = {}
    for name, _ in pairs(extra_defs.font_presets) do
        table.insert(t, name)
    end

    chosen_name = t[math.random(1, #t)]
end

local font_config = assert(extra_defs.font_presets[chosen_name], "no such font name \"" .. chosen_name .. "\"")
local xft_font = longFontFormat(font_config.name, font_config.terminal_pixelsize)

local enable_ligatures = true

local declOld = function(k, v)
    decl {
        {k, v},
        targets = {t_xres, t_dots},
    }
end

decl {
    {"st.alpha", "0.9"},
    {"st.cursor", theme["base0D"]},
    {"st.font", xft_font},
    {"st.enableligatures", (enable_ligatures and font_config.supports_ligatures) and 1 or 0},
    targets = {t_xres},
}

-- tym
declOld("tym.font", font_config.name .. " " .. font_config.terminal_pixelsize)

-- urxvt
declOld("uxrvt.font", "xft:" .. xft_font)
declOld("urxvt.scrollBar", "false")
declOld("urxvt.keysym.Meta-k", "command:\\033]720;1\\007")
declOld("urxvt.keysym.Meta-j", "command:\\033]721;1\\007")
declOld("urxvt.depth", "32")
declOld("urxvt.background", "[85]" .. theme["base00"])
declOld("urxvt.smoothResize", "true")

-- awesomewm
declOld("awesome.font", font_config.name .. " " .. font_config.awesome_size)
declOld("awesome.border-normal", theme["base00"])
declOld("awesome.border-focus", theme["base03"])
declOld("awesome.border-marked", theme["base0A"])

-- dwm
declOld("dwm.norm.bg", theme["base00"])
declOld("dwm.norm.fg", theme["base05"])
declOld("dwm.norm.border", theme["base00"])
declOld("dwm.sel.bg", theme["base02"])
declOld("dwm.sel.fg", theme["base05"])
declOld("dwm.sel.border", theme["base03"])
declOld("dwm.font", xft_font)

-- dmenu
declOld("dmenu.font", xft_font)
declOld("dmenu.norm.bg", theme["base00"])
declOld("dmenu.norm.fg", theme["base05"])
declOld("dmenu.norm.hl.bg", theme["base00"])
declOld("dmenu.norm.hl.fg", theme["base09"])
declOld("dmenu.sel.bg", theme["base02"])
declOld("dmenu.sel.fg", theme["base05"])
declOld("dmenu.sel.hl.bg", theme["base02"])
declOld("dmenu.sel.hl.fg", theme["base09"])
declOld("dmenu.out.bg", theme["base0F"])
declOld("dmenu.out.fg", theme["base0F"])

-- xterm
declOld("xterm.font", xft_font)

-- luakit
declOld("luakit.bg", theme["base00"])

-- qtile
declOld("qtile.font-family", font_config.name)
declOld("qtile.font-size", font_config.terminal_pixelsize)
declOld("qtile.border-focus", theme["base03"])
declOld("qtile.border-normal", theme["base00"])
declOld("qtile.bar.bg", theme["base00"])
declOld("qtile.bar.fg", theme["base00"])
declOld("qtile.bar.fg.inactive", theme["base03"])

-- qutebrowser
declOld("qutebrowser.font_size", font_config.qutebrowser_size)
declOld("qutebrowser.fonts.monospace", font_config.name)
declOld("qutebrowser.fonts.standard", font_config.name)
declOld("qutebrowser.fonts.sans-serif", "NotoSansMedium")
declOld("qutebrowser.fonts.serif", font_config.name)
declOld("qutebrowser.bg", theme["base00"])
declOld("qutebrowser.fg", theme["base05"])
declOld("qutebrowser.bg-alt", theme["base01"])
declOld("qutebrowser.fg-alt", theme["base04"])
declOld("qutebrowser.bg-attention", theme["base01"])
declOld("qutebrowser.fg-attention", theme["base04"])
declOld("qutebrowser.sel.bg", theme["base02"])
declOld("qutebrowser.sel.fg", theme["base05"])
declOld("qutebrowser.match.fg", theme["base09"])
declOld("qutebrowser.error", theme["base08"])
declOld("qutebrowser.warning", theme["base09"])
declOld("qutebrowser.info", theme["base0D"])
declOld("qutebrowser.success", theme["base0B"])

-- others
declOld("Emacs.font", font_config.name)
declOld("pencilwm.highlight", theme["base03"])
declOld("polybar.fontname", font_config.name)
declOld("polybar.fontsize", "9.0")
declOld("gtk3.font", font_config.name)

-- general
declOld("*background", theme["base00"])
declOld("*foreground", theme["base05"])
declOld("*bg", theme["base00"])
declOld("*fg", theme["base05"])
-- declOld("*cursor", theme["base0D"])
-- declOld("*cursorColor", theme["base0D"])

for i = 0, 9 do
    decl {
        {"*.color" .. i, theme["base0" .. i]},
        {"*.color0" .. i, theme["base0" .. i]},
        {"*.base0" .. i, theme["base0" .. i]},
        targets = {t_xres}
    }
end

decl {
    {"*.color10", theme["base0A"]},
    {"*.color11", theme["base0B"]},
    {"*.color12", theme["base0C"]},
    {"*.color13", theme["base0D"]},
    {"*.color14", theme["base0E"]},
    {"*.color15", theme["base0F"]},
    {"*.base0A", theme["base0A"]},
    {"*.base0B", theme["base0B"]},
    {"*.base0C", theme["base0C"]},
    {"*.base0D", theme["base0D"]},
    {"*.base0E", theme["base0E"]},
    {"*.base0F", theme["base0F"]},
    targets = {t_xres}
}

return
