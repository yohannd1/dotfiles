import subprocess as sp
import theme

def xgetres(*args):
    proc = sp.Popen(["xgetres", *args], encoding="UTF-8", stdout=sp.PIPE, stdin=sp.PIPE, stderr=sp.PIPE)
    proc.stdin.close()
    stdout = proc.stdout.read().strip()
    proc.stdout.close()
    proc.stderr.close()
    return stdout

c.downloads.location.directory = "~/inbox"

for (key, command) in [("D", "tab-close"),
                       ("U", "undo"),
                       ("d", "scroll-page 0 0.5"),
                       ("u", "scroll-page 0 -0.5"),
                       ("ç", "set-cmd-text :")]:
    config.bind(key, command)

theme.load(c, {
    "pallete": {
        "background": xgetres("qutebrowser.background"),
        "background-alt": xgetres("qutebrowser.background"),
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

        "background-attention": "#181920",
        "foreground-attention": "#ffffff",
        "comment": "#6272a4",
    },
    "spacing": {
        "vertical": 2,
        "horizontal": 2,
    },
})
