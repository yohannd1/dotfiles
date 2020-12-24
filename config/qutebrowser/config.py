import os
import subprocess as sp
import theme
from theme import ThemeConfig, Namespace, Font

bindings = [
    ("D", "tab-close"),
    ("U", "undo"),
    ("d", "scroll-page 0 0.5"),
    ("u", "scroll-page 0 -0.5"),
    ("ç", "set-cmd-text :"),
]

def main():
    c.downloads.location.directory = os.getenv("XDG_DOWNLOAD_DIR") or "~/inbox"
    for (k, cmd) in bindings:
        config.bind(k, cmd)

    monospace_font = Font(xgetres("qutebrowser.fonts.monospace",
                                  "Source Code Pro"))

    ThemeConfig(
        palette = Namespace(
            bg=xgetres("qutebrowser.bg"),
            fg=xgetres("qutebrowser.fg"),
            bg_alt=xgetres("qutebrowser.bg-alt"),
            fg_alt=xgetres("qutebrowser.fg-alt"),
            bg_attention=xgetres("qutebrowser.bg-attention"),
            fg_attention=xgetres("qutebrowser.fg-attention"),
            sel_fg=xgetres("qutebrowser.sel.fg"),
            sel_bg=xgetres("qutebrowser.sel.bg"),
            match_fg=xgetres("qutebrowser.match.fg"),
            error=xgetres("qutebrowser.error"),
            warning=xgetres("qutebrowser.warning"),
            info=xgetres("qutebrowser.info"),
            success=xgetres("qutebrowser.success"),
        ),
        spacing = Namespace(
            vertical=2,
            horizontal=2,
        ),
        fonts = {
            "main": monospace_font,
            "monospace": monospace_font,
            "standard": Font(xgetres("qutebrowser.fonts.standard",
                                     "Noto Sans Medium")),
            "sans_serif": Font(xgetres("qutebrowser.fonts.sans-serif",
                                       "Noto Sans Medium")),
            "serif": Font(xgetres("qutebrowser.fonts.serif",
                                  "Times New Roman")),
        },
        font_size = xgetres("qutebrowser.font_size", "10pt"),
    ).apply_to(c)

    c.downloads.open_dispatcher = os.environ.get("OPENER") or "xdg-open"
    c.colors.webpage.darkmode.enabled = False
    c.url.start_pages = ["qute://bookmarks/#bookmarks"]
    c.url.searchengines = {
        "DEFAULT": "https://duckduckgo.com/?q={}",
        "dg": "https://duckduckgo.com/?q={}",
        "gl": "https://google.com/search?q={}",
        "aw": "https://wiki.archlinux.org/?search={}",
        "ap": "https://archlinux.org/packages/?q={}",
        "yt": "https://www.youtube.com/results?search_query={}",
    }
    config.load_autoconfig()

def xgetres(resource, fallback=None):
    command = sp.run(["xgetres", resource], stdout=sp.PIPE, encoding="UTF-8").stdout.strip()
    if command == "":
        if fallback is not None:
            return fallback
        else:
            raise ValueError(f"X Resource '{resource}' seems to be undefined...")
    else:
        return command

main()
