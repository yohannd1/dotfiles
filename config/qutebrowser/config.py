import os
import subprocess as sp
import theme
from theme import ThemeConfig, Namespace, Font

QUTEBROWSER_SESSION_NAME = os.getenv("QUTEBROWSER_SESSION_NAME") or ""
is_default_session = (QUTEBROWSER_SESSION_NAME == "")

def main():
    c.downloads.location.directory = os.getenv("XDG_DOWNLOAD_DIR") or "~/inbox"

    bind = config.bind
    unbind = config.unbind

    bind("D", "tab-close")
    bind("U", "undo")
    bind("d", "scroll-page 0 0.5")
    bind("u", "scroll-page 0 -0.5")
    bind("ç", "cmd-set-text :")
    bind("E", "devtools right")
    unbind("<Ctrl-V>")
    bind("<Ctrl-Alt-I>", "mode-enter passthrough")

    for (key, dir) in [
        ("j", "down"), ("k", "up"),
        ("h", "left"), ("l", "right"),
    ]:
        bind(key, f"cmd-run-with-count 2 scroll {dir}")

    unbind("<Shift-Escape>", mode="passthrough")
    bind("<Ctrl-Alt-I>", "mode-leave", mode="passthrough")

    bind("<Ctrl-J>", "prompt-item-focus next", mode="prompt")
    bind("<Ctrl-K>", "prompt-item-focus prev", mode="prompt")

    monospace_font = Font(get_res("qutebrowser.fonts.monospace",
                                  "Source Code Pro"))

    ThemeConfig(
        palette=Namespace(
            bg=get_res("qutebrowser.bg"),
            fg=get_res("qutebrowser.fg"),
            bg_alt=get_res("qutebrowser.bg-alt"),
            fg_alt=get_res("qutebrowser.fg-alt"),
            bg_attention=get_res("qutebrowser.bg-attention"),
            fg_attention=get_res("qutebrowser.fg-attention"),
            sel_fg=get_res("qutebrowser.sel.fg"),
            sel_bg=get_res("qutebrowser.sel.bg"),
            match_fg=get_res("qutebrowser.match.fg"),
            error=get_res("qutebrowser.error"),
            warning=get_res("qutebrowser.warning"),
            info=get_res("qutebrowser.info"),
            success=get_res("qutebrowser.success"),
        ),
        spacing=Namespace(
            vertical=2,
            horizontal=2,
        ),
        fonts={
            "main": monospace_font,
            "monospace": monospace_font,
            "standard": Font(get_res("qutebrowser.fonts.standard",
                                     "NotoSansMedium")),
            "sans_serif": Font(get_res("qutebrowser.fonts.sans-serif",
                                       "NotoSansMedium")),
        },
        font_size = get_res("qutebrowser.font_size", "10pt"),
    ).apply_to(c)

    c.editor.command = ["graphedit", "{file}"]

    c.zoom.default = "90%"
    c.downloads.open_dispatcher = os.environ.get("OPENER") or "xdg-open"
    c.colors.webpage.darkmode.enabled = False
    c.url.searchengines = {
        "DEFAULT": "https://duckduckgo.com/?q={}",
        "dg": "https://duckduckgo.com/?q={}",
        "gl": "https://google.com/search?q={}",
        "aw": "https://wiki.archlinux.org/?search={}",
        "ap": "https://archlinux.org/packages/?q={}",
        "yt": "https://www.youtube.com/results?search_query={}",
        "def": "https://duckduckgo.com/?q={}&ia=definition",
    }

    config.load_autoconfig()

    allow_media_for("*://discord.com/*")
    allow_media_for("*://meet.google.com/*")

    if is_default_session:
        c.url.start_pages = [
            # "qute://bookmarks/#bookmarks",
            "https://discord.com/channels/@me",
            "https://web.whatsapp.com/",
            "https://app.schildi.chat/",
        ]

def allow_media_for(pattern: str):
    with config.pattern(pattern) as p:
        p.content.media.audio_video_capture = True
        p.content.autoplay = True

def get_res(resource, fallback=None):
    command = sp.run(["dotcfg", "send", f"get:{resource}"], stdout=sp.PIPE, encoding="UTF-8").stdout.strip()
    if command == "":
        if fallback is not None:
            return fallback
        else:
            raise ValueError(f"X Resource '{resource}' seems to be undefined...")
    else:
        return command

main()
