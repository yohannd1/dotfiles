from __future__ import annotations
from dataclasses import dataclass

from libqtile.command.base import expose_command
from libqtile.layout.base import _SimpleLayoutBase, Window
from libqtile.log_utils import logger
from libqtile.config import ScreenRect

# TODO: keep window widths on reset


@dataclass
class ClientAttrs:
    width: int | None = None


class Paper(_SimpleLayoutBase):
    defaults = [
        ("margin", 0, "Margin of the layout (int or list of ints [N E S W])"),
        ("border_focus", "#0000ff", "Border colour(s) for the window when focused"),
        (
            "border_normal",
            "#000000",
            "Border colour(s) for the window when not focused",
        ),
        ("border_width", 0, "Border width."),
        (
            "default_width_factor",
            0.65,
            "Default width for windows, as a factor of the screen width",
        ),
        (
            "max_if_single_window",
            False,
            "Maximize it if it is the only window available",
        ),
        ("center_all", False, "Automatically center the focused window"),
    ]

    def __init__(self, **config):
        _SimpleLayoutBase.__init__(self, **config)
        self.add_defaults(Paper.defaults)
        self.client_attrs = {}
        self.screen_rect = None

        self.last_client = None
        self.last_idx = None
        self.anchor = "center"
        self.pos_x = 0

    def configure(self, client: Window, screen_rect: ScreenRect) -> None:
        try:
            client_idx = self.clients.index(client)
        except ValueError:
            # layout not expecting this window so ignore it
            return

        # if it is the only window (and the option is on), just render the window as if we were on the Max layout
        if len(self.clients) == 1 and self.max_if_single_window:
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
        for i, c in enumerate(self.clients):
            attrs = self.client_attrs[c]
            if attrs.width is None:
                attrs.width = int(screen_rect.width * self.default_width_factor)

            # width_factor = self.width_factor_big if self.client_attrs[c].is_big else self.width_factor_small
            # width = int(width_factor * screen_rect.width - self.border_width * 2)
            client_rects[i] = (
                cur_x,
                0,
                attrs.width,
                screen_rect.height - self.border_width * 2,
            )
            cur_x += attrs.width

        # get the current client's centered position and use it to calculate the x offset
        assert self.clients.current_client is not None
        focus_idx = self.clients.index(self.clients.current_client)
        focus_rect = client_rects[focus_idx]

        self._update_anchor(focus_idx)
        self._update_position(focus_rect, screen_rect)

        # position the client in question according to its designated width, "abstract" location and offset
        (client_x, client_y, client_w, client_h) = client_rects[client_idx]
        offset_x = int(-self.pos_x)
        final_rect = (
            screen_rect.x + client_x + offset_x,
            screen_rect.y + client_y,
            client_w,
            client_h,
        )

        too_on_left = (final_rect[0] + final_rect[2]) < screen_rect.x
        too_on_right = final_rect[0] > screen_rect.x + screen_rect.width

        if too_on_left or too_on_right:
            client.hide()
        else:
            client.unhide()
            client.place(
                *final_rect,
                self.border_width,
                self.border_focus if client.has_focus else self.border_normal,
                margin=self.margin,
            )

        self.last_client = self.clients.current_client
        self.last_idx = focus_idx

    def _update_anchor(
        self,
        focus_idx: int,
    ) -> None:
        if self.center_all:
            self.anchor = "center"
            return

        if self.clients.current_client == self.last_client:
            if self.last_idx < focus_idx:
                self.anchor = "right"
            elif self.last_idx > focus_idx:
                self.anchor = "left"
            else:
                # nothing happened - keep the previous anchor
                pass
        else:
            try:
                last_client_cur_idx = self.clients.index(self.last_client)
                if last_client_cur_idx < focus_idx:
                    self.anchor = "right"
                elif last_client_cur_idx > focus_idx:
                    self.anchor = "left"
            except ValueError:
                # do nothing I guess?
                pass

    def _update_position(
        self,
        focus_rect: tuple[int, int, int, int],
        screen_rect: ScreenRect,
    ) -> None:
        (focus_x, _, focus_w, _) = focus_rect

        if self.anchor == "center":
            self.pos_x = focus_x - screen_rect.width / 2 + focus_w / 2
        elif self.anchor == "left":
            rightmost = focus_x
            self.pos_x = min(self.pos_x, rightmost)
        elif self.anchor == "right":
            leftmost = focus_x + focus_w - screen_rect.width + self.border_width
            self.pos_x = max(self.pos_x, leftmost)
        else:
            raise ValueError(f"unknown anchor: {self.anchor}")
        pass

    def add_client(
        self,
        client: Window,
        offset_to_current: int = 0,
        client_position: str | None = None,
    ) -> None:
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
        index = min(len(self.clients) - 1, index)  # force the index to be in range

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
        if attrs.width is None:
            logger.error(
                "tried to change width of a client whose width hasn't been yet calculated"
            )
            return
        attrs.width = max(0, attrs.width + amount)

        self.group.layout_all()  # toggle re-layout

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
        self.group.focus(self.clients[self.clients.current_client])

    @expose_command("toggle_focus_floating")
    def toggle_focus_floating(self) -> None:
        """Jumps the focus to the "floating lane", or returns from it."""

        if (cur_client := self.clients.current_client) is None:
            return

        cur_idx = self.group.windows.index(self.clients.current_client)
        desired_floating = not cur_client.floating
        predicate = lambda w: w.floating == desired_floating

        logger.warning(f"*** {self.group.windows}")

        idx_next = self._find_next_window(
            predicate, starting_index=cur_idx + 1, list_=self.group.windows
        )
        idx_prev = self._find_previous_window(
            predicate, starting_index=cur_idx - 1, list_=self.group.windows
        )

        logger.warning(f"{idx_next=} {idx_prev=}")

        if idx_next is None and idx_prev is None:
            pass
        elif idx_next is None:
            self.group.windows[idx_prev].focus()
        elif idx_prev is None:
            self.group.windows[idx_next].focus()
        else:
            if (idx_next - cur_idx) <= (cur_idx - prev_idx):
                self.group.windows[idx_next].focus()
            else:
                self.group.windows[idx_prev].focus()

    def _find_next_window(
        self,
        predicate: Callable[[Window], bool],
        starting_index: int | None = None,
        list_: Sequence[Window] | None = None,
    ) -> int | None:
        """Find the nearest next window that satisfies `predicate(window)`.
        `starting_index` defaults to the current window's index.
        If none were found, returns None.
        """

        if list_ is None:
            list_ = self.clients

        if starting_index is not None:
            i = starting_index
        elif isinstance(list_, self.clients.__class__):
            i = list_.current_index + 1
        else:
            raise ValueError("no way to figure out starting_index")

        while i < len(list_):
            if predicate(list_[i]):
                return i
            i += 1
        return None

    def _find_previous_window(
        self,
        predicate: Callable[[Window], bool],
        starting_index: int | None = None,
        list_: Sequence[Window] | None = None,
    ) -> int | None:
        """Find the nearest previous window that satisfies `predicate(window)`.
        `starting_index` defaults to the current window's index.
        If none were found, returns None.
        """

        if list_ is None:
            list_ = self.clients

        if starting_index is not None:
            i = starting_index
        elif isinstance(list_, self.clients.__class__):
            i = list_.current_index - 1
        else:
            raise ValueError("no way to figure out starting_index")

        while i >= 0:
            if predicate(list_[i]):
                return i
            i -= 1
        return None

    def focus_next(self, _win: Window) -> Window | None:
        """Focus the next window on the client list that has the same floating state as the current one.
        If no next one is found until the end, keeps the current index.
        """

        desired_floating = self.clients.current_client.floating
        predicate = lambda w: w.floating == desired_floating
        if (index := self._find_next_window(predicate)) is not None:
            return self.clients[index]
        else:
            return self.clients.current_client

    def focus_previous(self, _win: Window) -> Window | None:
        """Focus the previous window on the client list that has the same floating state as the current one.
        If no previous one is found until the start, keeps the current index.
        """

        desired_floating = self.clients.current_client.floating
        predicate = lambda w: w.floating == desired_floating
        if (index := self._find_previous_window(predicate)) is not None:
            return self.clients[index]
        else:
            return self.clients.current_client
