local awful = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup")
local gears = require("gears")

local mod = "Mod4"

local global_keys = gears.table.join(
    awful.key({mod}, "s",
              hotkeys_popup.show_help,
              {description = "show keybindings", group = "awesome"}),

    awful.key({mod}, "j",
              function() awful.client.focus.byidx(1) end,
              {description = "focus next by index", group = "client"}),

    awful.key({mod}, "k",
              function() awful.client.focus.byidx(-1) end,
              {description = "focus previous by index", group = "client"}),

    -- Layout manipulation
    awful.key({mod, "Shift"}, "j",
              function() awful.client.swap.byidx(  1)    end,
              {description = "swap with next client by index", group = "client"}),

    awful.key({mod, "Shift"}, "k",
              function() awful.client.swap.byidx( -1)    end,
              {description = "swap with previous client by index", group = "client"}),

    awful.key({mod, "Control"}, "j",
              function() awful.screen.focus_relative( 1) end,
              {description = "focus the next screen", group = "screen"}),

    awful.key({mod, "Control"}, "k",
              function() awful.screen.focus_relative(-1) end,
              {description = "focus the previous screen", group = "screen"}),

    awful.key({mod}, "u",
              awful.client.urgent.jumpto,
              {description = "jump to urgent client", group = "client"}),

    awful.key({mod, "Control", "Shift"}, "r",
              awesome.restart,
              {description = "reload awesome", group = "awesome"}),

    awful.key({mod, "Control", "Shift"}, "e",
              awesome.quit,
              {description = "quit awesome", group = "awesome"}),

    awful.key({mod}, "h",
              function() awful.tag.incmwfact(-0.05) end,
              {description = "decrease master width factor", group = "layout"}),

    awful.key({mod}, "l",
              function() awful.tag.incmwfact(0.05) end,
              {description = "increase master width factor", group = "layout"}),

    awful.key({mod, "Shift"}, "h",
              function() awful.tag.incnmaster(1, nil, true) end,
              {description = "increase the number of master clients", group = "layout"}),

    awful.key({mod, "Shift"}, "l",
              function() awful.tag.incnmaster(-1, nil, true) end,
              {description = "decrease the number of master clients", group = "layout"}),

    awful.key({mod, "Control"}, "h",
              function() awful.tag.incncol( 1, nil, true) end,
              {description = "increase the number of columns", group = "layout"}),

    awful.key({mod, "Control"}, "l",
              function() awful.tag.incncol(-1, nil, true) end,
              {description = "decrease the number of columns", group = "layout"}),

    awful.key({mod, "Shift"}, "Tab",
              function() awful.layout.inc(-1) end,
              {description = "select previous", group = "layout"}),

    awful.key({mod}, "Tab",
              function() awful.layout.inc(1) end,
              {description = "select next", group = "layout"}),

    awful.key({mod}, "x",
              function()
                  awful.prompt.run {
                    prompt       = "eval:",
                    textbox      = awful.screen.focused().mypromptbox.widget,
                    exe_callback = awful.util.eval,
                    history_path = awful.util.get_cache_dir() .. "/history_eval"
                  }
              end,
              {description = "lua execute prompt", group = "awesome"})
)

-- {{{ Key bindings

local client_keys = gears.table.join(
    awful.key({mod}, "f",
              function(c)
                  c.fullscreen = not c.fullscreen
                  c:raise()
              end,
              {description = "toggle fullscreen", group = "client"}),

    awful.key({mod}, "q",
              function(c) c:kill() end,
              {description = "close", group = "client"}),

    awful.key({mod, "Shift"}, "space",
              awful.client.floating.toggle,
              {description = "toggle floating", group = "client"}),

    awful.key({mod}, "p",
              function(c) c:swap(awful.client.getmaster()) end,
              {description = "move to master", group = "client"}),

    -- awful.key({mod}, "o",
    --           function(c) c:move_to_screen() end,
    --           {description = "move to screen", group = "client"}),

    awful.key({mod}, "t",
              function(c) c.ontop = not c.ontop end,
              {description = "toggle keep on top", group = "client"})

    -- awful.key({mod}, "n",
    --           function(c)
    --               -- The client currently has the input focus, so it cannot be
    --               -- minimized, since minimized clients can't have the focus.
    --               c.minimized = true
    --           end,
    --           {description = "minimize", group = "client"}),

    -- awful.key({mod}, "m",
    --           function(c)
    --               c.maximized = not c.maximized
    --               c:raise()
    --           end,
    --           {description = "(un)maximize", group = "client"}),

    -- awful.key({mod, "Control"}, "m",
    --           function(c)
    --               c.maximized_vertical = not c.maximized_vertical
    --               c:raise()
    --           end,
    --           {description = "(un)maximize vertically", group = "client"}),

    -- awful.key({mod, "Shift"}, "m",
    --           function(c)
    --               c.maximized_horizontal = not c.maximized_horizontal
    --               c:raise()
    --           end,
    --           {description = "(un)maximize horizontally", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    global_keys = gears.table.join(global_keys,
        -- View tag only.
        awful.key({mod}, "#" .. i + 9,
                  function()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  {description = "view tag #" .. i, group = "tag"}),

        -- Toggle tag display.
        awful.key({mod, "Control"}, "#" .. i + 9,
                  function()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = "toggle tag #" .. i, group = "tag"}),

        -- Move client to tag.
        awful.key({mod, "Shift"}, "#" .. i + 9,
                  function()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  {description = "move focused client to tag #" .. i, group = "tag"}),

        -- Toggle tag on focused client.
        awful.key({mod, "Control", "Shift"}, "#" .. i + 9,
                  function()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  {description = "toggle focused client on tag #" .. i, group = "tag"})
    )
end

local client_btn = {
    awful.button({}, 1,
                 function(c)
                     c:emit_signal("request::activate", "mouse_click", {raise = true})
                 end),
    awful.button({mod}, 1,
                 function(c)
                     c:emit_signal("request::activate", "mouse_click", {raise = true})
                     awful.mouse.client.move(c)
                 end),
    awful.button({mod}, 3,
                 function(c)
                     c:emit_signal("request::activate", "mouse_click", {raise = true})
                     awful.mouse.client.resize(c)
                 end)
}

-- Set keys
root.keys(global_keys)

-- }}}

return {
    global_keys = global_keys,
    client_keys = client_keys,
    client_btn = client_btn,
}
