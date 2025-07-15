import os, sys
from pathlib import Path

HOME = Path(os.environ["HOME"])

def eprint(*args, **kwargs) -> None:
    global sys
    print(*args, file=sys.stderr, **kwargs)

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
        # "i3",
        # "bspwm",

        "sxhkd",
        "MangoHud",
        "picom",
        "zathura",
        "taskwarrior",
        "sxiv",
        "tig",
        # "rofi",
        # "polybar",

        # "wezterm",
        # "alacritty",
    ]

    for i in {2, 4}:
        origin = DOTFILES / f"config/gtk-{i}.0"
        if origin.is_dir():
            m.link_glob(origin, f"~/.config/gtk-{i}.0")

apps += [
    "irb",
    "inputrc",
    "dircolors",
    "mimeapps.list",
    "pulsemixer.cfg",
    "ripgreprc",
    "dots",
    "mpv",
    # "lf",
    # "broot",
]

for app in apps:
    m.link_conf(app, f"~/.config/{app}")

m.link_conf("gdbinit", "~/.gdbinit")
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
m.link_conf("latexmkrc", "~/.latexmkrc")
m.link_conf("tmux.conf", "~/.tmux.conf")
m.link_conf("icons_default.theme", "~/.icons/default/index.theme")
m.link_conf("radium/keybindings.conf", "~/.radium/keybindings.conf")
m.link_conf("waybar/config", "~/.config/waybar/config")

# text editors
m.link_glob(DOTFILES / "config/kak", "~/.config/kak")
m.link_glob(DOTFILES / "config/nvim", "~/.config/nvim")
m.link_glob(DOTFILES / "config/emacs", "~/.emacs.d")

if not m.is_android:
    # desktop apps

    # FIXME: dynamically detect valid profile folders (not that hard I think...)
    m.link_conf("librewolf/overrides.js", "~/.librewolf/librewolf.overrides.cfg")
    m.link_conf("librewolf/userChrome.css", "~/.librewolf/slyys373.default-default/chrome/userChrome.css")

    m.link_glob(DOTFILES / "config/qutebrowser", "~/.config/qutebrowser")

    # m.link_glob(DOTFILES / "config/furnace", "~/.config/furnace") # new backup system is incompatible with this
    m.link_glob(DOTFILES / "desktop", "~/.local/share/applications")

    # m.link_glob(DOTFILES / "config/vscode", "~/.config/Code/User")
    m.link_glob(DOTFILES / "config/vscode", "~/.config/VSCodium/User")

try_xdg_cache_home = os.environ.get("XDG_CACHE_HOME")
XDG_CACHE_HOME = Path(try_xdg_cache_home) if try_xdg_cache_home is not None else (HOME / ".cache")

DOTS_CACHE = XDG_CACHE_HOME / "dots"

repos_dir = DOTS_CACHE / "repos"
repos_dir.mkdir(parents=True, exist_ok=True)
eprint("Downloading/updating repos...")

def set_up_repo(name: str, url: str) -> None:
    path = DOTS_CACHE / "repos" / name

    eprint(f"Will set up repo: {name} ({url}) into {path}")

    if path.exists():
        pass
    else:
        os.system(f"git clone {repr(url)} {repr(str(p))}")

set_up_flatcolor = False
if set_up_flatcolor:
    set_up_repo("FlatColor", "https://github.com/yohannd1/FlatColor-fork")
    themes_path = HOME / ".themes"
    themes_path.mkdir(parents=True, exist_ok=True)
    m.link_glob(DOTS_CACHE / "repos/FlatColor", themes_path / "FlatColor")

if os.system("which dotcfg >/dev/null 2>/dev/null") == 0:
    eprint("Generating config...", end="")
    os.system(DOTFILES / "scripts/gen-config")
    eprint(" done!")
else:
    eprint("dotcfg not found - skipping config generation. Please install it ASAP!")
