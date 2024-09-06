from libqtile.lazy import lazy
from libqtile.config import Key

from .utils import fzagnostic
from .data import Config

alt = "mod1"
mod = "mod4"
ctrl = "control"
shift = "shift"

def get_keys(config: Config):
    # Mostly just WM-related keybindings here. Use sxhkd for the rest.

    return [
        Key([mod], "j",
            lazy.group.next_window(),
            lazy.window.bring_to_front(),
            desc="Go to next window"),
        Key([mod], "k",
            lazy.group.prev_window(),
            lazy.window.bring_to_front(),
            desc="Go to previous window"),

        Key([mod], "h", lazy.layout.shrink_main(),
            desc="Shrink main window"),
        Key([mod], "l", lazy.layout.grow_main(),
            desc="Expand main window"),

        Key([mod, shift], "h", lazy.layout.shrink(),
            desc="Shrink current window"),
        Key([mod, shift], "l", lazy.layout.grow(),
            desc="Expand current window"),

        Key([mod, shift], "space", lazy.window.toggle_floating(),
            desc="toggle floating"),

        Key([mod, shift], "j", lazy.layout.shuffle_down(),
            desc="Move window down/forward"),
        Key([mod, shift], "k", lazy.layout.shuffle_up(),
            desc="Move window up/backward"),

        Key([mod], "n", lazy.layout.normalize(),
            desc="Reset all window sizes"),

        Key([mod], "Tab", lazy.next_layout(),
            desc="Go to next layout"),
        Key([mod, shift], "Tab", lazy.prev_layout(),
            desc="Go to previous layout"),

        Key([mod], "q", lazy.window.kill(),
            desc="Close focused window"),

        Key([mod, ctrl, alt], "r", lazy.restart(),
            desc="Restart Qtile"),
        Key([mod, ctrl, alt], "e", lazy.shutdown(),
            desc="Leave Qtile"),

        Key([mod, shift], "f", lazy.window.toggle_fullscreen(),
            desc="Toggle fullscreen window"),

        Key([mod], "Return", lazy.spawn(config.terminal),
            desc="Spawn terminal"),

        Key([mod], "u", manual_updates,
            desc="Manual update"),

        Key([mod, alt], "space", switch_to_window,
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
