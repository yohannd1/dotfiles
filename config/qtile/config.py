import os
from typing import List

from libqtile import bar, layout, widget, hook
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy

from settings.utils import get_res
from settings.keys import get_keys, mod
from settings.data import Config

cfg = Config(
    terminal=os.getenv("TERMINAL") or "xterm",
)

keys = get_keys(cfg)

groups = [Group(i) for i in "123456789"]

for i in groups:
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

layout_theme_cfg = dict(
    border_width=2,
    margin=2,
    border_focus=get_res("qtile.border-focus", "#e1acff"),
    border_normal=get_res("qtile.border-normal", "#1D2330"),
)

layouts = [
    layout.MonadTall(**layout_theme_cfg),
    # layout.MonadWide(**layout_theme_cfg),
    layout.Max(**layout_theme_cfg),
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
def client_focus(client):
    client.bring_to_front()

task_list: widget.TaskList = widget.TaskList(
    mouse_callbacks={
        "Button1": lambda: tasklist_window_select(task_list),
    },
    max_title_width=200,
    highlight_method="block",
)

standard_bar = bar.Bar(
    [
        widget.GroupBox(
            active=get_res("qtile.bar.fg"),
            inactive=get_res("qtile.bar.fg.inactive"),
            highlight_method="block",
            visible_groups=[],
        ),
        widget.TextBox("["),
        widget.CurrentLayout(),
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

        widget.Volume(fmt="vol: {}"),
        widget.TextBox("|"),
        widget.Clock(format="%Y-%m-%d %H:%M"),
        widget.Systray(),
    ],
    size=24,
    foreground=get_res("qtile.bar.fg", fallback="#FFFFFF"),
    background=get_res("qtile.bar.bg", fallback="#000000"),
)

screens = [Screen(bottom=standard_bar)]

# Drag floating layouts.
mouse = [
    Drag([mod],
         "Button1",
         lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod, "shift"],
         "Button1",
         lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
follow_mouse_focus = False
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(wm_class="float"),
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ]
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
