;; TODO: try out dynamic tagging: https://github.com/pw4ever/awesome-wm-config#persistent-dynamic-tagging
;; TODO: rootblocks integration (https://awesomewm.org/doc/api/classes/wibox.widget.textbox.html ?)
;; TODO: center new windows
;; TODO: add keybinding to minimize/unminimize window
;; TODO: fix - brave windows seem to be always floating
;; TODO: dismiss notification via left click
;; TODO: change layout indicator graphics
;; TODO: gaps configuration

(local user (assert _G.user "Failed to get `user` global variable"))

(set user.terminal (-> "TERMINAL" (os.getenv) (or "xterm")))
(set user.editor (-> "EDITOR" (os.getenv) (or "vi")))

;; check if LuaRocks is installed
(local has-luarocks (pcall require :luarocks.loader))

(local awesome _G.awesome)
(local naughty (require :naughty))
(local wibox (require :wibox))
(local awful (require :awful))
(local beautiful (require :beautiful))
(local menubar (require :menubar))
(local gears (require :gears))
(local fennel/view (require :fennel.view))

;; Automatically focus windows in some specific contexts.
;; Not requiring this renders the WM unusable for me. Not exactly sure why yet.
(require :awful.autofocus)

;; load the theme
(beautiful.init (user.loadFnlConfig "theme.fnl"))

;; handle runtime errors after startup
(do
  (var is-handling-error false)

  (awesome.connect_signal "debug::error"
    (fn [err]
      (when (not is-handling-error)
        (set is-handling-error true)

        (naughty.notify {:preset naughty.config.presets.critical
                         :title "An error happened!"
                         :text (tostring err)})

        (set is-handling-error false)))))

;; load keybindings
(local {: mod-key
        : ctrl
        : client-keys
        : client-buttons
        : global-keys
        : client/toggle-minimize}
  (user.loadFnlConfig :kb-config.fnl))

;; set global keys
(_G.root.keys global-keys)

;; set the list of layouts to be used - order matters!
(set awful.layout.layouts [awful.layout.suit.tile
                           awful.layout.suit.max])

(set menubar.utils.terminal user.terminal)

(do ; wibar configuration
  (local text-clock (wibox.widget.textclock))
  (set text-clock.format " %a, %Y-%m-%d %H:%M ")

  (local taglist-buttons
    (gears.table.join
      (awful.button [] 1 #($1:view_only))
      (awful.button [mod-key] 1
                    #(when client.focus
                       (client.focus:move_to_tag $1)))
      (awful.button [] 3 awful.tag.viewtoggle)
      (awful.button [mod-key] 3
                    #(when client.focus
                       (client.focus:toggle_tag $1)))
      (awful.button [] 4 #(awful.tag.viewprev $1.screen))
      (awful.button [] 5 #(awful.tag.viewnext $1.screen))
      ))

  (local tasklist-buttons
    (gears.table.join
      (awful.button [] 1 (fn [c]
                           (if (= c client.focus)
                             nil ; (set c.minimized true)
                             (c:emit_signal "request::activate" "tasklist"
                                            {:raise true}))))
      (awful.button [ctrl] 1 client/toggle-minimize)
      (awful.button [] 3 #(awful.menu.client_list {:theme {:width 250}}))
      (awful.button [] 4 #(awful.client.focus.byidx -1))
      (awful.button [] 5 #(awful.client.focus.byidx +1))
      ))

  (local tag-names (icollect [_ n (ipairs [1 2 3 4 5 6 7 8 9])]
                     (string.format "%s" n)))

  (awful.screen.connect_for_each_screen
    (fn [screen]
      (each [_ name (ipairs tag-names)]
        ;; Useful reference: https://awesomewm.org/doc/api/classes/tag.html
        (awful.tag.add name {:gap 3
                             :screen screen
                             :layout (. awful.layout.layouts 1)}))

      (local prompt-box (awful.widget.prompt))
      (set screen.prompt-box prompt-box)

      ;; create a widget to display the current layout's icon
      (local layout-box (awful.widget.layoutbox screen))
      (layout-box:buttons
        (gears.table.join
          (awful.button [] 1 #(awful.layout.inc +1))
          (awful.button [] 3 #(awful.layout.inc -1))
          (awful.button [] 4 #(awful.layout.inc +1))
          (awful.button [] 5 #(awful.layout.inc -1))))

      (local tag-list
        (awful.widget.taglist {: screen
                               :filter awful.widget.taglist.filter.noempty
                               :buttons taglist-buttons
                               :style {:shape gears.shape.circle
                                       :width 10
                                       :height 10}
                               :layout {:layout wibox.layout.fixed.horizontal
                                        :spacing 2}}))

      (local task-list
        (awful.widget.tasklist
          {: screen
           :filter awful.widget.tasklist.filter.currenttags
           :buttons tasklist-buttons
           :style {:shape gears.shape.rounded_bar
                   :spacing 10}
           :layout {:spacing 10
                    :spacing_widget {1 {:forced_width 2
                                        :shape gears.shape.circle
                                        :color "#00000000"
                                        :widget wibox.widget.separator}
                                     :valign "center"
                                     :halign "center"
                                     :bg "#00000000"
                                     :widget wibox.container.place}
                    :layout wibox.layout.flex.horizontal}
           :widget_template {1 {1 {1 {:id "text_role"
                                      :widget wibox.widget.textbox}
                                   :layout wibox.layout.fixed.horizontal}
                                :left 5
                                :right 5
                                :widget wibox.container.margin}
                             :id "background_role"
                             :widget wibox.container.background}}))

      (local top-bar (awful.wibar {: screen
                                   :bg "#00000000"
                                   :position "top"
                                   :height 25}))

      (top-bar:setup
        {:widget wibox.container.margin
         :top 5
         :left 5
         :right 5
         1 {:layout wibox.layout.align.horizontal
            1 {1 {:layout wibox.layout.fixed.horizontal
                     1 tag-list
                     2 prompt-box}
                  :widget wibox.container.background
                  :bg beautiful.bg_normal
                  :shape gears.shape.rounded_bar}

            2 {1 task-list
               :left 5
               :right 5
               :widget wibox.container.margin}

            3 {1 {1 {:layout wibox.layout.fixed.horizontal
                        1 (wibox.widget.systray)
                        2 text-clock
                        3 layout-box}
                     :left 10
                     :right 10
                     :widget wibox.container.margin}
                  :widget wibox.container.background
                  :bg beautiful.bg_normal
                  :shape gears.shape.rounded_bar}
            }}

        )

      (set _G.top-bar top-bar))))

(do ; rules
  (set awful.rules.rules
       [{:rule {} ; all clients will match this rule
         :properties {:border_width beautiful.border_width
                      :border_color beautiful.border_color
                      :titlebars_enabled false
                      :focus awful.client.focus.filter
                      :raise true
                      :keys client-keys
                      :buttons client-buttons
                      :screen awful.screen.preferred
                      :placement (+ awful.placement.no_overlap
                                    awful.placement.no_offscreen)}}
        {:rule_any {:instance ["pinentry"]
                    :class ["Sxiv" "float" "Pavucontrol" "Gnome-pomodoro"]
                    :name ["Event Tester" "Steam - News" "Krita - Edit Text — Krita"]
                    :role ["pop-up"]}
         :properties {:floating true}}
        ]))

(do ; signals
  ;; Helpful reference:
  ;; https://www.qubes-os.org/doc/awesomewm/

  (client.connect_signal
    "manage" ; when a new client appears
    #(when (and awesome.startup
                (not $1.size_hints.user_position)
                (not $1.size_hints.program_position))
       ; prevent clients from being unreachable after screen count changes
       ; TODO: what does this mean?
       (awful.placement.no_offscreen $1)))

  ; (client.connect_signal
  ;   "mouse::enter" ; when the mouse enters a window
  ;   #($1:emit_signal "request::activate" "mouse_enter" {:raise false}))

  (client.connect_signal
    "focus" ; when a window is focused
    #(do
       (set $1.border_color beautiful.border_focus)))

  (client.connect_signal
    "unfocus" ; when a window is unfocused
    #(set $1.border_color beautiful.border_normal))

  (client.connect_signal
    "request::titlebars" ; when titlebars are enabled on a client
    (fn [clt]
      (local buttons
        (gears.table.join
          (awful.button [] 1 #(do
                                (clt:emit_signal "request::activate" "titlebar" {:raise true})
                                (awful.mouse.client.move clt)))
          (awful.button [] 3 #(do
                                (clt:emit_signal "request::activate" "titlebar" {:raise true})
                                (awful.mouse.client.resize clt)))
          ))

      (-> (awful.titlebar clt)
          (: :setup {1 {1 (awful.titlebar.widget.iconwidget clt)
                       :buttons buttons
                       :layout wibox.layout.fixed.horizontal}
                     2 {1 {:align "center"
                           :widget (awful.titlebar.widget.titlewidget clt)}
                        :buttons buttons
                        :layout wibox.layout.flex.horizontal}
                     3 (gears.table.join
                         [(awful.titlebar.widget.floatingbutton  clt)
                          (awful.titlebar.widget.maximizedbutton clt)
                          (awful.titlebar.widget.stickybutton    clt)
                          (awful.titlebar.widget.ontopbutton     clt)
                          (awful.titlebar.widget.closebutton     clt)]
                         {:layout (wibox.layout.fixed.horizontal)})
                     :layout {:spacing 10
                              :spacing_widget {1 {:forced_width 2
                                                  :shape gears.shape.circle
                                                  :color beautiful.fg_minimize
                                                  :widget wibox.widget.separator}
                                     :valign "center"
                                     :halign "center"
                                     :widget wibox.container.place}
                    :layout wibox.layout.flex.horizontal}                     })

          )
      )))
