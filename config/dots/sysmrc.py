if m.is_android:
    m.link_conf("termux", "~/.termux")
else:
    apps = (
        "bspwm",
        "polybar",
        "sxhkd",
        "alacritty",
        "picom",
        "xorg",
        "awesome",
        "zathura",
        "taskwarrior",
        "sxiv",
        # "i3",
        # "rofi",

        "inputrc",
        "dircolors",
        "mimeapps.list",
        "ripgreprc",
        "dots",
        "lf",
        "broot",
    )

    for app in apps:
        m.link_conf(app, f"~/.config/{app}")

    for i in {2, 3, 4}:
        origin = DOTFILES / f"config/gtk-{i}.0"
        if origin.is_dir():
            m.link_glob(origin, f"~/.config/gtk-{i}.0")

    m.link_conf("profile", "~/.profile")
    m.link_conf("profile", "~/.zprofile")
    m.link_conf("zsh/zshrc", "~/.config/zsh/.zshrc")
    m.link_conf("zsh/zlogout", "~/.config/zsh/.zlogout")
    m.link_conf("git/gitconfig", "~/.gitconfig")
    m.link_conf("git/gitignore", "~/.gitignore")

    m.link_glob(DOTFILES / "config/nvim", "~/.config/nvim")
    m.link_glob(DOTFILES / "config/emacs", "~/.emacs.d")
    m.link_glob(DOTFILES / "desktop", "~/.local/share/applications")
    # m.link_glob(DOTFILES / "config/kak", "~/.config/kak")
    # m.link_glob(DOTFILES / "config/vscode", "~/.config/Code/User")

os.system(DOTFILES / "scripts/gen-config")
