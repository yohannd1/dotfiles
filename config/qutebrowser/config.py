import subprocess as sp
import theme

def command_output(*args):
    proc = sp.Popen(args, encoding="UTF-8", stdout=sp.PIPE)
    stdout = proc.stdout.read().strip()
    proc.stdout.close()
    proc.kill()
    return stdout

xgetres = lambda res: command_output("xgetres", res)

c.downloads.location.directory = "~/inbox"

for bind_tuple in [("D", "tab-close"),
                   ("U", "undo"),
                   ("d", "scroll-page 0 0.5"),
                   ("u", "scroll-page 0 -0.5")]:
    config.bind(*bind_tuple)

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
