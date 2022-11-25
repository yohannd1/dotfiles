local meta = _G._meta
local theme = meta.theme
local decl = meta.decl

local function basename(path)
    return path:match("(.*/)")
end

local this_path = debug.getinfo(1, 'S').source:gsub("^@", "")
local this_basename = basename(this_path)

local extra_defs = loadfile(this_basename .. "/" .. "res_extra_defs.lua")()
local longFontFormat = extra_defs.longFontFormat

local chosen_name = "FantasqueSans"
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

-- st
decl("st.alpha", "0.9")
decl("st.cursor", theme["base0D"])
decl("st.font", xft_font)
decl("st.enableligatures", (enable_ligatures and font_config.supports_ligatures) and 1 or 0)

-- tym
decl("tym.font", font_config.name .. " " .. font_config.terminal_pixelsize)

-- urxvt
decl("uxrvt.font", "xft:" .. xft_font)
decl("urxvt.scrollBar", "false")
decl("urxvt.keysym.Meta-k", "command:\\033]720;1\\007")
decl("urxvt.keysym.Meta-j", "command:\\033]721;1\\007")
decl("urxvt.depth", "32")
decl("urxvt.background", "[85]" .. theme["base00"])
decl("urxvt.smoothResize", "true")

-- awesomewm
decl("awesome.font", font_config.name .. " " .. font_config.awesome_size)
decl("awesome.border-normal", theme["base00"])
decl("awesome.border-focus", theme["base03"])
decl("awesome.border-marked", theme["base0A"])

-- dwm
decl("dwm.norm.bg", theme["base00"])
decl("dwm.norm.fg", theme["base05"])
decl("dwm.norm.border", theme["base00"])
decl("dwm.sel.bg", theme["base02"])
decl("dwm.sel.fg", theme["base05"])
decl("dwm.sel.border", theme["base03"])
decl("dwm.font", xft_font)

-- dmenu
decl("dmenu.font", xft_font)
decl("dmenu.norm.bg", theme["base00"])
decl("dmenu.norm.fg", theme["base05"])
decl("dmenu.norm.hl.bg", theme["base00"])
decl("dmenu.norm.hl.fg", theme["base09"])
decl("dmenu.sel.bg", theme["base02"])
decl("dmenu.sel.fg", theme["base05"])
decl("dmenu.sel.hl.bg", theme["base02"])
decl("dmenu.sel.hl.fg", theme["base09"])
decl("dmenu.out.bg", theme["base0F"])
decl("dmenu.out.fg", theme["base0F"])

-- xterm
decl("xterm.font", xft_font)

-- luakit
decl("luakit.bg", theme["base00"])

-- qtile
decl("qtile.font-family", font_config.name)
decl("qtile.font-size", font_config.terminal_pixelsize)
decl("qtile.border-focus", theme["base03"])
decl("qtile.border-normal", theme["base00"])
decl("qtile.bar.bg", theme["base00"])
decl("qtile.bar.fg", theme["base00"])
decl("qtile.bar.fg.inactive", theme["base03"])

-- qutebrowser
decl("qutebrowser.font_size", font_config.qutebrowser_size)
decl("qutebrowser.fonts.monospace", font_config.name)
decl("qutebrowser.fonts.standard", font_config.name)
decl("qutebrowser.fonts.sans-serif", "NotoSansMedium")
decl("qutebrowser.fonts.serif", font_config.name)
decl("qutebrowser.bg", theme["base00"])
decl("qutebrowser.fg", theme["base05"])
decl("qutebrowser.bg-alt", theme["base01"])
decl("qutebrowser.fg-alt", theme["base04"])
decl("qutebrowser.bg-attention", theme["base01"])
decl("qutebrowser.fg-attention", theme["base04"])
decl("qutebrowser.sel.bg", theme["base02"])
decl("qutebrowser.sel.fg", theme["base05"])
decl("qutebrowser.match.fg", theme["base09"])
decl("qutebrowser.error", theme["base08"])
decl("qutebrowser.warning", theme["base09"])
decl("qutebrowser.info", theme["base0D"])
decl("qutebrowser.success", theme["base0B"])

-- others
decl("Emacs.font", font_config.name)
decl("pencilwm.highlight", theme["base03"])
decl("polybar.fontname", font_config.name)
decl("polybar.fontsize", "9.0")
decl("gtk3.font", font_config.name)

-- general
decl("*background", theme["base00"])
decl("*foreground", theme["base05"])
decl("*bg", theme["base00"])
decl("*fg", theme["base05"])
-- decl("*cursor", theme["base0D"])
-- decl("*cursorColor", theme["base0D"])

for i = 0, 9 do
    decl("*.color" .. i, theme["base0" .. i])
    decl("*.color0" .. i, theme["base0" .. i])
    decl("*.base0" .. i, theme["base0" .. i])
end

decl("*.color10", theme["base0A"])
decl("*.color11", theme["base0B"])
decl("*.color12", theme["base0C"])
decl("*.color13", theme["base0D"])
decl("*.color14", theme["base0E"])
decl("*.color15", theme["base0F"])
decl("*.base0A", theme["base0A"])
decl("*.base0B", theme["base0B"])
decl("*.base0C", theme["base0C"])
decl("*.base0D", theme["base0D"])
decl("*.base0E", theme["base0E"])
decl("*.base0F", theme["base0F"])
