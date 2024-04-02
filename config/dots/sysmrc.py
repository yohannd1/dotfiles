import os, sys
from pathlib import Path

apps = []

if m.is_android:
    m.link_glob(DOTFILES/"config/termux", "~/.termux")
else:
    apps += [
        "xorg",
        "river",
        "hypr",
        "awesome",
        "qtile",

        "sxhkd",
        "picom",
        "zathura",
        "taskwarrior",
        "sxiv",
        "tig",
        # "i3",
        # "rofi",
        # "bspwm",
        # "polybar",
        # "alacritty",
    ]

    # for i in {2, 4}:
    #     origin = DOTFILES / f"config/gtk-{i}.0"
    #     if origin.is_dir():
    #         m.link_glob(origin, f"~/.config/gtk-{i}.0")

apps += [
    "irb",
    "inputrc",
    "dircolors",
    "mimeapps.list",
    "pulsemixer.cfg",
    "ripgreprc",
    "dots",
    "lf",
    # "broot",
]

for app in apps:
    m.link_conf(app, f"~/.config/{app}")

m.link_conf("gdbinit", f"~/.gdbinit")
m.link_conf("profile", "~/.profile")
m.link_conf("guile/guilerc", "~/.guile")
m.link_conf("bash/bashrc", "~/.bashrc")
m.link_conf("profile", "~/.zprofile")
m.link_conf("zsh/zshrc", "~/.config/zsh/.zshrc")
m.link_conf("zsh/zlogout", "~/.config/zsh/.zlogout")
m.link_conf("git/gitconfig", "~/.gitconfig")
m.link_conf("git/gitignore", "~/.gitignore")
m.link_conf("cargo-config.toml", "~/.cache/cargo/config.toml")
m.link_conf("pylintrc", "~/.pylintrc")
m.link_conf("tmux.conf", "~/.tmux.conf")
m.link_conf("icons_default.theme", "~/.icons/default/index.theme")
m.link_conf("radium/keybindings.conf", "~/.radium/keybindings.conf")
m.link_conf("waybar/config", "~/.config/waybar/config")

m.link_glob(DOTFILES / "config/qutebrowser", "~/.config/qutebrowser")
# m.link_glob(DOTFILES / "config/furnace", "~/.config/furnace") # new backup system is incompatible with this
m.link_glob(DOTFILES / "config/nvim", "~/.config/nvim")
m.link_glob(DOTFILES / "config/emacs", "~/.emacs.d")
m.link_glob(DOTFILES / "desktop", "~/.local/share/applications")
m.link_glob(DOTFILES / "config/kak", "~/.config/kak")
# m.link_glob(DOTFILES / "config/vscode", "~/.config/Code/User")

XDG_CACHE_HOME = Path(os.environ["XDG_CACHE_HOME"])
DOTS_CACHE = XDG_CACHE_HOME / "dots"

print("Downloading/updating repos...", file=sys.stderr)

(DOTS_CACHE / "repos").mkdir(parents=True, exist_ok=True)
REPOS = {
    # "FlatColor": "https://github.com/YohananDiamond/FlatColor",
}
for (name, url) in REPOS.items():
    print(f"Current repo: {name} ({url})", file=sys.stderr)
    p = DOTS_CACHE / "repos" / name
    if p.exists():
        # os.system(f"cd {repr(str(p))} && git pull")
        pass
    else:
        os.system(f"git clone {repr(url)} {repr(str(p))}")

Path("~/.themes").expanduser().mkdir(parents=True, exist_ok=True)
m.link_glob(DOTS_CACHE / "repos/FlatColor", "~/.themes/FlatColor")

print("Generating config...", file=sys.stderr, end="")

# general config
os.system(DOTFILES / "scripts/gen-config")

print(" done!", file=sys.stderr)
