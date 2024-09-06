from __future__ import annotations
from dataclasses import dataclass

from libqtile.command.base import expose_command
from libqtile.layout.base import _SimpleLayoutBase
from libqtile.log_utils import logger

# TODO: keep window widths on reset
# TODO: niri-like horizontal scrolling offset (don't necessarily center the windows)
# TODO: hide windows that aren't visible (I suppose that improves performance?)

@dataclass
class ClientAttrs:
    width: int | None = None

class Paper(_SimpleLayoutBase):
    defaults = [
        ("margin", 0, "Margin of the layout (int or list of ints [N E S W])"),
        ("border_focus", "#0000ff", "Border colour(s) for the window when focused"),
        ("border_normal", "#000000", "Border colour(s) for the window when not focused"),
        ("border_width", 0, "Border width."),
        ("default_width_factor", 0.65, "Default width for windows, as a factor of the screen width"),
        ("max_if_only", False, "Maximize it if it is the only window available")
    ]

    def __init__(self, **config):
        _SimpleLayoutBase.__init__(self, **config)
        self.add_defaults(Paper.defaults)
        self.client_attrs = {}
        self.screen_rect = None

    def configure(self, client: Window, screen_rect: ScreenRect) -> None:
        try:
            client_idx = self.clients.index(client)
        except ValueError:
            # layout not expecting this window so ignore it
            return

        # if it is the only window (and the option is on), just render the window as if we were on the Max layout
        if len(self.clients) == 1 and self.max_if_only:
            client.unhide()
            client.place(
                screen_rect.x,
                screen_rect.y,
                screen_rect.width - 2 * self.border_width,
                screen_rect.height - 2 * self.border_width,
                self.border_width,
                self.border_focus if client.has_focus else self.border_normal,
                margin=self.margin,
            )
            return

        # populate the clients list
        # TODO: optimize this (find a way to only calculate this when windows change position, size, order etc.)
        client_rects = [(0, 0, 0, 0) for _ in range(len(self.clients))]
        cur_x = 0
        for (i, c) in enumerate(self.clients):
            attrs = self.client_attrs[c]
            if attrs.width is None:
                attrs.width = int(screen_rect.width * self.default_width_factor)

            # width_factor = self.width_factor_big if self.client_attrs[c].is_big else self.width_factor_small
            # width = int(width_factor * screen_rect.width - self.border_width * 2)
            client_rects[i] = (cur_x, 0, attrs.width, screen_rect.height - self.border_width * 2)
            cur_x += attrs.width

        # get the current client's centered position and use it to calculate the render offset
        focus_idx = self.clients.index(self.clients.current_client)
        (focus_x, _, focus_w, _) = client_rects[focus_idx]
        # render_offset_x = int(focus_x + focus_w / 2)
        render_offset_x = int(-focus_x + (screen_rect.width - focus_w) / 2) # i'm not sure how to explain this...

        # position the client in question according to its designated width, "abstract" location and offset
        (client_x, client_y, client_w, client_h) = client_rects[client_idx]
        client.unhide()
        client.place(
            screen_rect.x + client_x + render_offset_x,
            screen_rect.y + client_y,
            client_w,
            client_h,
            self.border_width,
            self.border_focus if client.has_focus else self.border_normal,
            margin=self.margin,
        )

    def add_client(self, client: Window) -> None:
        self.client_attrs[client] = ClientAttrs(width=None)
        return super().add_client(client, 1)

    def remove(self, client: Window) -> Window | None:
        # stop tracking info about that client
        if client in self.client_attrs:
            del self.client_attrs[client]

        # client is not even in the list
        if client not in self.clients:
            return None

        # client is only window in the list
        if len(self.clients) == 1:
            self.clients.remove(client)
            return None

        index = self.clients.index(client)
        self.clients.remove(client)
        index = min(len(self.clients) - 1, index) # force the index to be in range

        new_focus = self.clients[index]
        return new_focus

    @expose_command("shrink")
    def shrink(self, amount: int = 100) -> None:
        self.modify_width(-amount)

    @expose_command("grow")
    def grow(self, amount: int = 100) -> None:
        self.modify_width(amount)

    @expose_command("modify_width")
    def modify_width(self, amount: int) -> None:
        if self.clients.current_client is None:
            return

        attrs = self.client_attrs[self.clients.current_client]
        attrs.width = max(0, attrs.width + amount)

        self.group.layout_all() # toggle re-layout

    @expose_command()
    def shuffle_up(self):
        """Shuffle the client up the stack"""
        self.clients.shuffle_up()
        self.group.layout_all()
        self.group.focus(self.clients.current_client)

    @expose_command()
    def shuffle_down(self):
        """Shuffle the client down the stack"""
        self.clients.shuffle_down()
        self.group.layout_all()
        self.group.focus(self.clients[self.focused])
