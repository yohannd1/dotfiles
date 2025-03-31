import os
import re
from typing import List

from libqtile import bar, layout, widget, hook
from libqtile.config import Group, Key, Match, Screen
from libqtile.lazy import lazy

from settings.utils import get_res
from settings.keys import make_keyboard_map, make_mouse_map, mod
from settings.data import Config
from layouts.paper import Paper

groups = [Group(i) for i in "123456789"]

cfg = Config(
    terminal=os.getenv("TERMINAL") or "xterm",
    groups=groups,
)

keys = make_keyboard_map(cfg)
mouse = make_mouse_map(cfg)

layout_theme_cfg = dict(
    margin=7,
    border_width=2,
    border_focus=get_res("qtile.border-focus", "#e1acff"),
    border_normal=get_res("qtile.border-normal", "#1D2330"),
)

layouts = [
    Paper(
        default_width_factor=0.5,
        max_if_single_window=True,
        center_all=False,
        **layout_theme_cfg,
    ),
    # layout.MonadTall(**layout_theme_cfg),
    layout.Max(**layout_theme_cfg),
    # layout.MonadWide(**layout_theme_cfg),
    # layout.Zoomy(**layout_theme_cfg),
    # layout.Columns(border_focus_stack='#d75f5f'),
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
]

widget_defaults = dict(
    font=get_res("qtile.font-family", fallback="sans"),
    fontsize=int(float(get_res("qtile.font-size", fallback="12"))),
    foreground=get_res("qtile.bar.fg", fallback="#000000"),
    padding=3,
)
extension_defaults = widget_defaults.copy()

def tasklist_window_select(tl):
    if tl.clicked:
        window = tl.clicked
        window.group.focus(window, False)

@hook.subscribe.client_focus
def on_client_focus(window) -> None:
    # XXX: this doesn't work well with some GTK popups (specially on VirtualBox)
    # at least checking if it's floating minimizes the issue (?)
    if window.floating:
        window.bring_to_front()

task_list = widget.TaskList(
    mouse_callbacks={
        "Button1": lambda: tasklist_window_select(task_list),
    },
    max_title_width=500,
    highlight_method="block",
    margin_y=0,
    margin_x=20,
    stretch=True,
    icon_size=0,
)

standard_bar = bar.Bar(
    [
        widget.GroupBox(
            active=get_res("qtile.bar.fg"),
            inactive=get_res("qtile.bar.fg.inactive"),
            highlight_method="block",
            visible_groups=[],
            disable_drag=True,
        ),
        widget.TextBox("["),
        widget.CurrentLayout(),
        widget.CurrentScreen(
            active_color=get_res("qtile.bar.fg"),
            inactive_color=get_res("theme.base03"),
        ),
        widget.TextBox("]"),

        task_list,
        # widget.WindowName(),

        # widget.Chord(
        #     chords_colors={
        #         "launch": ("#ff0000", "#ffffff"),
        #     },
        #     name_transform=lambda name: name.upper(),
        # ),
        # widget.TextBox("|"),
        # vol_widget,
        # widget.TextBox("|"),
        # widget.CPU( # depends on psutil
        #     update_interval=5.0,
        #     format='CPU {freq_current}GHz {load_percent:02.0f}%',
        # ),
        # widget.TextBox("|"),
        # widget.Battery(
        #     format="BAT {percent:.0%}",
        # ),

        # out-volume (mic)
        widget.Volume(
            fmt="mic: {}",
            get_volume_command=r""" pactl get-source-volume @DEFAULT_SOURCE@ | awk '{ print $5 }' """,
            check_mute_command=r""" pactl get-source-mute @DEFAULT_SOURCE@ | awk '{ print $2 }' """,
            check_mute_string="yes",
        ),
        widget.TextBox("|"),

        # in-volume (speakers)
        widget.Volume(
            fmt="spk: {}",
            get_volume_command=r""" pactl list sinks | grep '^[[:space:]]Volume:' | awk '{ print $5 }' """,
            check_mute_command=r""" pactl list sinks | grep '^[[:space:]]Mute:' | awk '{ print $2 }' """,
            check_mute_string="yes",
        ),
        widget.TextBox("|"),

        widget.Clock(format="%Y-%m-%d %H:%M"),
        widget.Systray(),
    ],
    size=25,
    foreground=get_res("qtile.bar.fg", fallback="#FFFFFF"),
    background=get_res("qtile.bar.bg", fallback="#000000"),
)

screens = [Screen(top=standard_bar)]

unity_matchers = [
    re.compile(r"^UnityEditor\."),
    re.compile(r"^Select "),
]

def is_unity_floating(win) -> bool:
    wm_class = win.get_wm_class()

    if not wm_class or wm_class[0] != "Unity":
        return False

    for r in unity_matchers:
        if r.match(win.name) is not None:
            return True

    return False

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
follow_mouse_focus = False
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,

        Match(wm_class="confirmreset"), # gitk
        Match(wm_class="makebranch"), # gitk
        Match(wm_class="maketag"), # gitk
        Match(title="branchdialog"), # gitk

        Match(wm_class="ssh-askpass"), # ssh-askpass
        Match(wm_class="float"), # generic floating stuff
        Match(title="pinentry"), # GPG key password entry

        Match(func=is_unity_floating), # Unity Editor
    ],
    **layout_theme_cfg,
)
auto_fullscreen = False
focus_on_window_activation = "smart"

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
