local M = {}

M.font_presets = {
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
        base_size = 13,
    },
    ["JetbrainsMono"] = {
        name = "JetBrains Mono",
        base_size = 13,
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
        base_size = 16,
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
}

M.longFontFormat = function(font_name, pixel_size)
    return string.format(
        "%s:pixelsize=%s:antialias=true:autohint=true;",
        font_name,
        pixel_size
    )
end

return M
