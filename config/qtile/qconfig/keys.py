from libqtile.lazy import lazy
from libqtile.config import Key

from .utils import fzagnostic

alt = "mod1"
mod = "mod4"
ctrl = "control"

def get_keys(terminal: str):
    return [
        Key([mod], "j", lazy.layout.down(),
            desc="Move focus down"),
        Key([mod], "k", lazy.layout.up(),
            desc="Move focus up"),

        Key([mod], "h", lazy.layout.shrink_main(),
            desc="Shrink main window"),
        Key([mod], "l", lazy.layout.grow_main(),
            desc="Expand main window"),

        Key([mod, "shift"], "h", lazy.layout.shrink(),
            desc="Shrink current window"),
        Key([mod, "shift"], "l", lazy.layout.grow(),
            desc="Expand current window"),

        Key([mod], "space", lazy.window.spawn("runnsend error-and-output fzrun"),
            desc="spawn fzrun"),
        Key([mod, "shift"], "space", lazy.window.toggle_floating(),
            desc="toggle floating"),

        Key([mod, "shift"], "j", lazy.layout.shuffle_down(),
            desc="Move window down"),
        Key([mod, "shift"], "k", lazy.layout.shuffle_up(),
            desc="Move window up"),

        Key([mod], "n", lazy.layout.normalize(),
            desc="Reset all window sizes"),

        Key([mod], "Tab", lazy.next_layout(),
            desc="Go to next layout"),
        Key([mod, "shift"], "Tab", lazy.prev_layout(),
            desc="Go to previous layout"),

        Key([mod], "q", lazy.window.kill(),
            desc="Close focused window"),

        Key([mod, ctrl, alt], "r", lazy.restart(),
            desc="Restart Qtile"),
        Key([mod, ctrl, alt], "e", lazy.shutdown(),
            desc="Leave Qtile"),

        # FIXME: fullscreen seems glitchy
        Key([mod], "f", lazy.window.toggle_fullscreen(),
            desc="Toggle fullscreen window"),

        Key([mod], "Return", lazy.spawn(terminal),
            desc="Toggle fullscreen window"),

        Key([mod], "u", manual_updates,
            desc="Manual update"),

        Key([mod], "s", switch_to_window,
            desc="Switch to window"),
    ]

@lazy.function
def manual_updates(qtile):
    for window in qtile.current_group.windows:
        if window.floating:
            window.cmd_bring_to_front()

@lazy.function
def switch_to_window(qtile):
    windows = list(qtile.current_group.windows) # just in case I end up causing some kind of deadlock

    choice = fzagnostic(
        choices=[w.info()["name"] for w in windows],
        prompt="Window:",
    )

    if choice is not None:
        i, _ = choice
        window = windows[i]
        window.group.focus(window, False)
