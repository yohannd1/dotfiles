import subprocess as sp
import theme
from theme import ThemeOpt, Palette

bindings = [
    ("D", "tab-close"),
    ("U", "undo"),
    ("d", "scroll-page 0 0.5"),
    ("u", "scroll-page 0 -0.5"),
    ("ç", "set-cmd-text :"),
]

def main():
    c.downloads.location.directory = "~/inbox"
    for (k, cmd) in bindings:
        config.bind(k, cmd)

    theme.load(c, {
        ThemeOpt.PALETTE: Palette(
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
        ThemeOpt.SPACING: {
            "vertical": 2,
            "horizontal": 2,
        },
    })

def xgetres(resource):
    command = sp.run(["xgetres", resource], stdout=sp.PIPE, encoding="UTF-8").stdout.strip()
    if command == "":
        raise ValueError(f"Resource '{resource}' seems to be undefined...")
    else:
        return command

main()
