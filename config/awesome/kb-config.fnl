(local export {})

(local awesome _G.awesome)
(local awful (require :awful))
(local gears (require :gears))
(local hotkeys-popup (require :awful.hotkeys_popup))
(local fennel (require :fennel))
(local fennel/view (require :fennel.view))
(local naughty (require :naughty))

(set _G.spawn (fn [s] (awful.spawn.with_shell s)))

(set export.mod-key "Mod4")
(local {: mod-key} export)

(set export.alt "Mod1")
(local {: alt} export)

(set export.shift "Shift")
(local {: shift} export)

(set export.ctrl "Control")
(local {: ctrl} export)

(fn export.client/toggle-minimize [c]
  (if (= c client.focus)
    (set c.minimized true)
    (c:emit_signal "request::activate" "tasklist" {:raise true})))

(->> [(awful.key [mod-key] "s" hotkeys-popup.show_help
                  {:description "show keybindings"
                   :group "awesome"})
       (awful.key [mod-key ctrl shift] "r" awesome.restart
                  {:description "reload awesome"
                   :group "awesome"})
       (awful.key [mod-key ctrl alt] "e" awesome.quit
                  {:description "quit awesome"
                   :group "awesome"})

       (awful.key [mod-key] "j" #(awful.client.focus.byidx +1)
                  {:description "focus next client"
                   :group "client"})
       (awful.key [mod-key] "k" #(awful.client.focus.byidx -1)
                  {:description "focus previous client"
                   :group "client"})
       (awful.key [mod-key shift] "j" #(awful.client.swap.byidx +1)
                  {:description "swap client with next"
                   :group "client"})
       (awful.key [mod-key shift] "k" #(awful.client.swap.byidx -1)
                  {:description "swap client with previous"
                   :group "client"})
       (awful.key [mod-key] "u" awful.client.urgent.jumpto
                  {:description "jump to urgent client"
                   :group "client"})

       (awful.key [mod-key ctrl] "j" #(awful.screen.focus_relative +1)
                  {:description "focus next screen"
                   :group "screen"})
       (awful.key [mod-key ctrl] "k" #(awful.screen.focus_relative -1)
                  {:description "focus previous screen"
                   :group "screen"})

       (awful.key [mod-key] "h" #(awful.tag.incmwfact -0.05)
                  {:description "decrease master width factor"
                   :group "layout"})
       (awful.key [mod-key] "l" #(awful.tag.incmwfact +0.05)
                  {:description "increase master width factor"
                   :group "layout"})
       (awful.key [mod-key shift] "h" #(awful.tag.incnmaster +1 nil true)
                  {:description "increase the number of master clients"
                   :group "layout"})
       (awful.key [mod-key shift] "l" #(awful.tag.incnmaster -1 nil true)
                  {:description "decrease the number of master clients"
                   :group "layout"})
       (awful.key [mod-key ctrl] "h" #(awful.tag.incncol +1 nil true)
                  {:description "increase the number of columns"
                   :group "layout"})
       (awful.key [mod-key ctrl] "l" #(awful.tag.incncol -1 nil true)
                  {:description "decrease the number of columns"
                   :group "layout"})
       (awful.key [mod-key shift] "Tab" #(awful.layout.inc -1)
                  {:description "previous layout"
                   :group "layout"})
       (awful.key [mod-key] "Tab" #(awful.layout.inc +1)
                  {:description "next layout"
                   :group "layout"})

       (awful.key [mod-key ctrl shift] "s" #(awful.spawn.with_shell "start-sxhkd standard & notify-send 'sxhkd restarted!'")
                  {:description "restart sxhkd"
                   :group "layout"})

       (awful.key [mod-key ctrl shift] "Return" #(awful.spawn.with_shell "st")
                  {:description "spawn terminal"
                   :group "layout"})

       (awful.key [mod-key] "f"
                  #(let [top-bar _G.top-bar
                         bottom-bar _G.bottom-bar]
                     (when top-bar (set top-bar.visible (not top-bar.visible)))
                     (when bottom-bar (set bottom-bar.visible (not bottom-bar.visible)))))

       (awful.key [mod-key] "x"
                  #(awful.prompt.run
                     {:prompt "eval(fnl): "
                      :textbox (-> (awful.screen.focused)
                                   (. :prompt-box :widget))
                      :exe_callback #(let [result (fennel.eval $1 user.fennel_load_opts)]
                                       (naughty.notify {:title "eval(fnl) output"
                                                        :text (fennel/view result)}))
                      :history_path (string.format "%s/eval-fnl-hist"
                                                   (awful.util.get_cache_dir))})
                  {:description "show fennel eval prompt"
                   :group "misc"})

       (awful.key [mod-key ctrl] "x"
                  #(awful.prompt.run
                     {:prompt "eval(lua): "
                      :textbox (-> (awful.screen.focused)
                                   (. :prompt-box :widget))
                      :exe_callback #(let [result (awful.util.eval $1)]
                                       (naughty.notify {:title "eval(lua) output"
                                                        :text (fennel/view result)}))
                      :history_path (string.format "%s/eval-lua-hist"
                                                   (awful.util.get_cache_dir))})
                  {:description "show lua eval prompt"
                   :group "misc"})
      ]
     (table.unpack)
     (gears.table.join)
     (set export.global-keys))

;; Tag-related bindings
;;
;; Warning: this uses keycodes to make it work on any keyboard layout.
;; This should map on the top row of your keyboard - usually 1 to 9.
(for [i 1 9]
  (set export.global-keys
       (gears.table.join
         export.global-keys

         ;; view specific tag
         (awful.key [mod-key] (.. "#" (+ i 9))
                    #(let [screen (awful.screen.focused)
                           tag (. screen.tags i)]
                       (when tag
                         (tag:view_only)))
                    {:description (string.format "view tag #%d" i)
                     :group "tag"})

         ;; toggle tag display
         (awful.key [mod-key ctrl] (.. "#" (+ i 9))
                    #(let [screen (awful.screen.focused)
                           tag (. screen.tags i)]
                       (when tag
                         (awful.tag.viewtoggle tag)))
                    {:description (string.format "toggle tag #%d" i)
                     :group "tag"})

         ;; move client to tag
         (awful.key [mod-key shift] (.. "#" (+ i 9))
                    #(when client.focus
                       (match (. client.focus.screen.tags i)
                         nil nil
                         tag (client.focus:move_to_tag tag)))
                    {:description (string.format "move focused client to tag #%d" i)
                     :group "tag"})

         ;; toggle tag on focused client
         (awful.key [mod-key ctrl shift] (.. "#" (+ i 9))
                    #(when client.focus
                       (match (. client.focus.screen.tags i)
                         nil nil
                         tag (client.focus:toggle_tag tag)))
                    {:description (string.format "toggle tag #%d on focused client" i)
                     :group "tag"})
         )))

(->> [(awful.key [mod-key shift] "f"
                 #(do
                    (set $1.fullscreen (not $1.fullscreen))
                    ($1:raise))
                 {:description "toggle fullscreen"
                  :group "client"})

      (awful.key [mod-key] "m"
                 export.client/toggle-minimize
                 {:description "toggle minimize"
                  :group "client"})

      (awful.key [mod-key] "q"
                 #($1:kill)
                 {:description "close window"
                  :group "client"})

      (awful.key [mod-key shift] "space"
                 awful.client.floating.toggle
                 {:description "toggle floating"
                  :group "client"})

      (awful.key [mod-key] "p"
                 #($1:swap (awful.client.getmaster))
                 {:description "move to master"
                  :group "client"})

      (awful.key [mod-key] "t"
                 #(set $1.ontop (not $1.ontop))
                 {:description "toggle keep on top"
                  :group "client"})
      ]
     (table.unpack)
     (gears.table.join)
     (set export.client-keys))

(->> [(awful.button [] 1
                    #($1:emit_signal "request::activate" "mouse_click" {:raise true}))
      (awful.button [mod-key] 1
                    #(do
                       ($1:emit_signal "request::activate" "mouse_click" {:raise true})
                       (set $1.floating true)
                       (awful.mouse.client.move $1)))
      (awful.button [mod-key] 2
                    awful.client.floating.toggle)
      (awful.button [mod-key shift] 1
                    #(do
                       ($1:emit_signal "request::activate" "mouse_click" {:raise true})
                       (set $1.floating true)
                       (awful.mouse.client.resize $1 "bottom_right")))
      ]
     (table.unpack)
     (gears.table.join)
     (set export.client-buttons))

export
