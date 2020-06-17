import subprocess as sp
import theme
from theme import ThemeOpt

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
        ThemeOpt.PALETTE: {
            "background": xgetres("qutebrowser.color0"),
            "background-alt": xgetres("qutebrowser.color0"),
            "border": xgetres("qutebrowser.border"),
            "current-line": xgetres("qutebrowser.selection"),
            "selection": xgetres("qutebrowser.selection"),
            "foreground": xgetres("qutebrowser.foreground"),
            "foreground-alt": xgetres("qutebrowser.foreground"),
            "red": xgetres("qutebrowser.color1"),
            "green": xgetres("qutebrowser.color2"),
            "orange": xgetres("qutebrowser.color3"),
            "yellow": xgetres("qutebrowser.color3"),
            "pink": xgetres("qutebrowser.color5"),
            "cyan": xgetres("qutebrowser.color6"),
            "purple": xgetres("qutebrowser.color4"),

            "background-attention": xgetres("qutebrowser.color0"),
            "foreground-attention": "#ffffff",
            "comment": "#6272a4",
        },
        ThemeOpt.SPACING: {
            "vertical": 2,
            "horizontal": 2,
        },
    })

def xgetres(*args):
    proc = sp.Popen(
        ["xgetres", *args],
        encoding="UTF-8",
        stdout=sp.PIPE,
        stdin=sp.PIPE,
        stderr=sp.PIPE,
    )
    proc.stdin.close()
    stdout = proc.stdout.read().strip()
    proc.stdout.close()
    proc.stderr.close()
    return stdout

main()
