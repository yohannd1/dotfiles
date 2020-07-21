# ColorSystem Valid values:
#   - rgb: Interpolate in the RGB color system.
#   - hsv: Interpolate in the HSV color system.
#   - hsl: Interpolate in the HSL color system.
#   - none: Don't show a gradient.

class Palette:
    def __init__(self, **kwargs):
        for (k, v) in kwargs.items():
            self.__setattr__(k, v)

def hex2rgba(hex, alpha):
    return "rgba(" + ", ".join([str(eval("0x" + hex[1:3])), str(eval("0x" + hex[3:5])), str(eval("0x" + hex[5:7])), str(alpha)]) + ")"

class ThemeOpt:
    PALETTE = 0
    SPACING = 1
    PADDING = 2

def load(c, options = {}):
    spacing = options[ThemeOpt.SPACING]
    palette = options[ThemeOpt.PALETTE]
    p = palette

    # Padding
    padding = options.get(ThemeOpt.PADDING, {
        "top": spacing["vertical"],
        "right": spacing["horizontal"],
        "bottom": spacing["vertical"],
        "left": spacing["horizontal"]
    })

    # Completion widget - category headers
    c.colors.completion.category.bg = p.bg_attention
    c.colors.completion.category.fg = p.fg_alt
    c.colors.completion.category.border.bottom = p.bg
    c.colors.completion.category.border.top = p.bg

    # Completion widget - general
    c.colors.completion.fg = p.fg
    c.colors.completion.odd.bg = p.bg # odd rows bg
    c.colors.completion.even.bg = p.bg_alt # even rows bg

    # Completion widget - selected row
    c.colors.completion.item.selected.bg = p.sel_bg
    c.colors.completion.item.selected.fg = p.sel_fg
    c.colors.completion.item.selected.border.bottom = p.sel_bg
    c.colors.completion.item.selected.border.top = p.sel_bg

    # Completion widget - matched text
    c.colors.completion.match.fg = p.match_fg

    # Completion widget - scrollbar
    c.colors.completion.scrollbar.fg = p.fg
    c.colors.completion.scrollbar.bg = p.bg

    # Download bar
    c.colors.downloads.bar.bg = p.bg
    c.colors.downloads.error.fg = p.error
    c.colors.downloads.error.bg = p.bg

    # Download bar - gradient
    c.colors.downloads.start.fg = p.fg
    c.colors.downloads.start.bg = p.bg
    c.colors.downloads.stop.fg = p.sel_fg
    c.colors.downloads.stop.bg = p.sel_bg

    c.colors.downloads.system.bg = "rgb" # see ColorSystem valid values

    # Hints
    c.colors.hints.fg = p.fg_alt
    c.colors.hints.bg = p.bg_alt
    c.hints.border = "1px solid " + p.bg
    c.colors.hints.match.fg = p.match_fg

    # Keyhint widget
    c.colors.keyhint.fg = p.fg
    c.colors.keyhint.bg = hex2rgba(p.bg, "80%")
    c.colors.keyhint.suffix.fg = p.match_fg # for keys to complete the current keychain

    # Error message
    c.colors.messages.error.fg = p.error
    c.colors.messages.error.bg = p.bg
    c.colors.messages.error.border = p.bg_alt

    # Info message
    c.colors.messages.info.fg = p.info
    c.colors.messages.info.bg = p.bg
    c.colors.messages.info.border = p.bg_alt

    # Warning message
    c.colors.messages.warning.fg = p.warning
    c.colors.messages.warning.bg = p.bg
    c.colors.messages.warning.border = p.bg_alt

    # Prompts
    c.colors.prompts.fg = p.fg_alt
    c.colors.prompts.bg = p.bg
    c.colors.prompts.border = "1px solid " + p.bg_alt
    c.colors.prompts.selected.bg = p.sel_bg

    # Statusbar
    c.statusbar.padding = padding
    c.colors.statusbar.normal.fg = p.fg
    c.colors.statusbar.normal.bg = p.bg
    c.colors.statusbar.caret.bg = p.bg_alt
    c.colors.statusbar.caret.fg = p.fg
    c.colors.statusbar.caret.selection.bg = p.bg_alt
    c.colors.statusbar.caret.selection.fg = p.fg_alt
    c.colors.statusbar.command.bg = p.bg
    c.colors.statusbar.command.fg = p.fg
    c.colors.statusbar.insert.bg = p.bg
    c.colors.statusbar.insert.fg = p.success
    c.colors.statusbar.passthrough.bg = p.bg
    c.colors.statusbar.passthrough.fg = p.warning
    c.colors.statusbar.private.bg = p.bg_alt
    c.colors.statusbar.private.fg = p.info
    c.colors.statusbar.command.private.bg = p.bg
    c.colors.statusbar.command.private.fg = p.info
    c.colors.statusbar.progress.bg = p.info
    c.colors.statusbar.url.fg = p.fg
    c.colors.statusbar.url.hover.fg = p.match_fg
    c.colors.statusbar.url.error.fg = p.error
    c.colors.statusbar.url.success.http.fg = p.fg
    c.colors.statusbar.url.success.https.fg = p.success
    c.colors.statusbar.url.warn.fg = p.warning

    c.colors.tabs.bar.bg = p.bg
    c.colors.tabs.even.bg = p.bg
    c.colors.tabs.even.fg = p.sel_fg
    c.colors.tabs.odd.bg = p.bg
    c.colors.tabs.odd.fg = p.sel_fg
    c.colors.tabs.selected.even.bg = p.sel_bg
    c.colors.tabs.selected.even.fg = p.fg
    c.colors.tabs.selected.odd.bg = p.sel_bg
    c.colors.tabs.selected.odd.fg = p.fg

    # Tab bar - indicator (for page loading)
    c.colors.tabs.indicator.error = p.error
    c.colors.tabs.indicator.start = p.warning
    c.colors.tabs.indicator.stop = p.success
    c.colors.tabs.indicator.system = "none" # see ColorSystem valid values

    # Tab padding
    c.tabs.padding = padding
    c.tabs.indicator.width = 1
    c.tabs.favicons.scale = 1
