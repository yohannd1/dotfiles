from libqtile.lazy import lazy
from libqtile.config import Key, Click, Drag

from .utils import fzagnostic
from .data import Config

alt = "mod1"
mod = "mod4"
ctrl = "control"
shift = "shift"

next_window_actions = [
    lazy.group.next_window(),
    lazy.window.bring_to_front(),
]

prev_window_actions = [
    lazy.group.prev_window(),
    lazy.window.bring_to_front(),
]

def make_keyboard_map(cfg) -> list:
    keys = []

    keys += [
        Key([mod], "j",
            *next_window_actions,
            desc="Go to next window"),
        Key([mod], "k",
            *prev_window_actions,
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

        Key([mod], "Return", lazy.spawn(cfg.terminal),
            desc="Spawn terminal"),

        Key([mod], "u", manual_updates,
            desc="Manual update"),

        Key([mod, alt], "space", switch_to_window,
            desc="Switch to window"),
    ]

    for i in cfg.groups:
        keys += [
            # mod1 + letter of group = switch to group
            Key([mod], i.name, lazy.group[i.name].toscreen(),
                desc="Switch to group {}".format(i.name)),

            # mod1 + shift + letter of group = switch to & move focused window to group
            Key([mod, "shift"], i.name, lazy.window.togroup(i.name, switch_group=False),
                desc="Move focused window to group {}".format(i.name)),
        ]

    for vt in range(1, 8):
        keys.append(Key(
            ["control", "mod1"],
            f"f{vt}",
            lazy.core.change_vt(vt).when(func=lambda: qtile.core.name == "wayland"),
            desc=f"Switch to VT{vt}",
        ))

    return keys

def make_mouse_map(cfg) -> list:
    return [
        Click([mod], "Button5",
              *next_window_actions),

        Click([mod], "Button4",
              *prev_window_actions),

        Drag([mod], "Button1",
             lazy.window.set_position_floating(),
             start=lazy.window.get_position()),

        Drag([mod, "shift"], "Button1",
             lazy.window.set_size_floating(),
             start=lazy.window.get_size()),
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
