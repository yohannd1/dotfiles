local M = {}

M.font_presets = {
    ["Iosevka"] = {
        name = "Iosevka Medium",
        terminal_pixelsize = "15",
        awesome_size = "12px",
        qutebrowser_size = "10pt",
        supports_ligatures = true, -- FIXME: I don't think so?
    },
    ["FiraCode"] = {
        name = "Fira Code Medium",
        terminal_pixelsize = "15",
        awesome_size = "12px",
        qutebrowser_size = "10pt",
        supports_ligatures = true,
    },
    ["CascadiaCode"] = {
        name = "Cascadia Code",
        terminal_pixelsize = "14",
        awesome_size = "12.5px",
        qutebrowser_size = "10pt",
        supports_ligatures = true,
    },
    ["JetbrainsMono"] = {
        name = "JetBrains Mono Medium",
        terminal_pixelsize = "15",
        awesome_size = "12.5px",
        qutebrowser_size = "10pt",
        supports_ligatures = true,
    },
    ["Fixedsys"] = {
        name = "Fixedsys Excelsior",
        terminal_pixelsize = "18",
        awesome_size = "14px",
        qutebrowser_size = "10pt",
        supports_ligatures = false, -- because of glitches
    },
    ["UbuntuMono"] = {
        name = "Ubuntu Mono",
        terminal_pixelsize = "17",
        awesome_size = "10",
        qutebrowser_size = "10pt",
        supports_ligatures = false,
    },
    ["RobotoMono"] = {
        name = "Roboto Mono Medium",
        terminal_pixelsize = "14",
        awesome_size = "12px",
        qutebrowser_size = "10pt",
        supports_ligatures = true,
    },
    ["SpaceMono"] = {
        name = "Space Mono",
        terminal_pixelsize = "14",
        awesome_size = "12px",
        qutebrowser_size = "10pt",
        supports_ligatures = false, -- because of glitches
    },
    ["FantasqueSans"] = {
        name = "Fantasque Sans Mono",
        terminal_pixelsize = "16",
        awesome_size = "13px",
        qutebrowser_size = "10pt",
        supports_ligatures = false, -- FIXME: check
    },
    ["Sudo"] = {
        name = "Sudo",
        terminal_pixelsize = "18",
        awesome_size = "16px",
        qutebrowser_size = "10pt",
        supports_ligatures = false,
    },
    ["Mononoki"] = {
        name = "Mononoki",
        terminal_pixelsize = "16",
        awesome_size = "12px",
        qutebrowser_size = "10pt",
        supports_ligatures = true,
    },
    ["Hack"] = {
        name = "Hack",
        terminal_pixelsize = "14",
        awesome_size = "12.5px",
        qutebrowser_size = "10pt",
        supports_ligatures = true,
    },
    ["IbmPlex"] = {
        name = "Ibm Plex Mono",
        terminal_pixelsize = "14",
        awesome_size = "12.5px",
        qutebrowser_size = "10pt",
        supports_ligatures = true,
    },
    ["ShareTech"] = {
        name = "Share Tech Mono",
        terminal_pixelsize = "16",
        awesome_size = "12px",
        qutebrowser_size = "10pt",
        supports_ligatures = false,
    },
    ["Unifont"] = {
        name = "Unifont",
        terminal_pixelsize = "16",
        awesome_size = "14px",
        qutebrowser_size = "10pt",
        supports_ligatures = true,
    },
    ["ProggyVector"] = {
        name = "ProggyVector",
        terminal_pixelsize = "14",
        awesome_size = "12px",
        qutebrowser_size = "10pt",
        supports_ligatures = false,
    },
    ["PtMono"] = {
        name = "PTMono",
        terminal_pixelsize = "14",
        awesome_size = "11px",
        qutebrowser_size = "10pt",
        supports_ligatures = false,
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
