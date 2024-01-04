-- vim: fdm=marker foldenable
-- FONT DEFS {{{
local font_presets = {
    ["SourceCodePro"] = {
        name = "Source Code Pro",
        base_size = 14,
    },
    ["Iosevka"] = {
        name = "Iosevka Medium",
        base_size = 15,
        supports_ligatures = true, -- FIXME: I don't think so?
    },
    ["FiraCode"] = {
        name = "Fira Code",
        base_size = 13,
    },
    ["CascadiaCode"] = {
        name = "Cascadia Code",
        base_size = 15,
    },
    ["JetbrainsMono"] = {
        name = "JetBrains Mono",
        base_size = 13.5,
    },
    ["Fixedsys"] = {
        name = "Fixedsys Excelsior",
        base_size = 16,
        supports_ligatures = false, -- because of glitches
    },
    ["UbuntuMono"] = {
        name = "Ubuntu Mono",
        base_size = 15,
        supports_ligatures = false,
    },
    ["RobotoMono"] = {
        name = "Roboto Mono Medium",
        base_size = 13,
    },
    ["SpaceMono"] = {
        name = "Space Mono",
        base_size = 13,
        supports_ligatures = false, -- because of glitches (FIXME: confirm)
    },
    ["FantasqueSans"] = {
        name = "Fantasque Sans Mono",
        base_size = 16,
    },
    ["Sudo"] = {
        name = "Sudo",
        base_size = 17,
    },
    ["Mononoki"] = {
        name = "Mononoki",
        base_size = 14,
    },
    ["Hack"] = {
        name = "Hack",
        base_size = 13,
    },
    ["IbmPlex"] = {
        name = "Ibm Plex Mono",
        base_size = 13,
    },
    ["ShareTech"] = {
        name = "Share Tech Mono",
        base_size = 15,
    },
    ["Unifont"] = {
        name = "Unifont",
        base_size = 15,
    },
    ["ProggyVector"] = {
        name = "ProggyVector",
        base_size = 13,
    },
    ["PtMono"] = {
        name = "PTMono",
        base_size = 14,
    },
    ["Hermit"] = {
        name = "Hermit",
        base_size = 13,
    },
    ["Agave"] = {
        name = "Agave",
        base_size = 15,
    },
    ["EnvyCodeR"] = {
        name = "Envy Code R",
        base_size = 15,
    },
    ["DinaRemasterII"] = {
        name = "DinaRemasterII",
        base_size = 16,
    },
    ["Bedstead"] = {
        name = "Bedstead",
        base_size = 14,
    },
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

    local supports_ligatures = val.supports_ligatures
    if supports_ligatures == nil then
        supports_ligatures = true
    end

    return {
        name = assert(val.name, "missing font name"),
        base_size = (size_multiplier or 1.0) * assert(val.base_size, "missing base_size"),
        supports_ligatures = supports_ligatures,
    }
end

local T_ALL = {t_xres, t_dots}
-- }}}

local enable_ligatures = false
local font = getFontInfo("JetbrainsMono", 1.15)

local fsize_term = font.base_size
local xft_font = longFontFormat(font.name, fsize_term)

-- st (terminal)
decl {
    {"st.alpha", "0.7"},
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

-- yambar
decl {
    {"yambar.font", longFontFormat(font.name, font.base_size * 0.95)},

    targets = T_ALL,
}

-- waybar
decl {
    {"waybar.font_family", font.name},
    {"waybar.font_size", font.base_size * 0.9},

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
    {"qtile.font-size", font.base_size},
    {"qtile.border-focus", theme["base03"]},
    {"qtile.border-normal", theme["base00"]},
    {"qtile.bar.bg", theme["base00"]},
    {"qtile.bar.fg", theme["base00"]},
    {"qtile.bar.fg.inactive", theme["base03"]},

    targets = T_ALL,
}

-- qutebrowser
decl {
    {"qutebrowser.font_size", "10pt"}, -- placeholder for when I use qutebrowser again someday
    {"qutebrowser.fonts.monospace", font.name},
    {"qutebrowser.fonts.standard", font.name},
    {"qutebrowser.fonts.sans-serif", "NotoSansMedium"},
    {"qutebrowser.fonts.serif", font.name},
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
