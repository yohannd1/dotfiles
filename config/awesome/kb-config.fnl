(local export {})

(local awesome _G.awesome)
(local awful (require :awful))
(local gears (require :gears))
(local hotkeys-popup (require :awful.hotkeys_popup))
(local fennel (require :fennel))

(set export.mod-key "Mod4")
(local mod-key export.mod-key)
(local shift "Shift")
(local ctrl "Control")

; (fn focus-by-idx [idx]
;   (match client.focus
;     nil (match (. (client.get) 1)
;           nil (awful.client.focus.byidx 0)
;           )
;     (awful.client.focus.byidx idx)
;     clt

;     )

;   (if client.focus
;     (awful.client.focus.byidx idx)
;     (when (-> (client.get) (length) (> 0))
;       (awful.client.focus.byidx 0
;                                 (. (client.get) 1)))))

(->> [(awful.key [mod-key] "s" hotkeys-popup.show_help
                  {:description "show keybindings"
                   :group "awesome"})
       (awful.key [mod-key ctrl shift] "r" awesome.restart
                  {:description "reload awesome"
                   :group "awesome"})
       (awful.key [mod-key ctrl shift] "e" awesome.quit
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

       (awful.key [mod-key ctrl] "h" #(awful.tag.incmwfact -0.05)
                  {:description "decrease master width factor"
                   :group "layout"})
       (awful.key [mod-key ctrl] "l" #(awful.tag.incmwfact +0.05)
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

       (awful.key [mod-key] "x"
                  #(awful.prompt.run
                     {:prompt "eval(fnl): "
                      :textbox (-> (awful.screen.focused)
                                   (. :prompt-box :widget))
                      :exe_callback #(fennel.eval $1 user.fennel_load_opts)
                      :history_path (string.format "%s/eval-fnl-hist"
                                                   (awful.util.get_cache_dir))})
                  {:description "show fennel eval prompt"
                   :group "misc"})

       (awful.key [mod-key ctrl] "x"
                  #(awful.prompt.run
                     {:prompt "eval(lua): "
                      :textbox (-> (awful.screen.focused)
                                   (. :prompt-box :widget))
                      :exe_callback awful.util.eval
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

(set export.client-keys
     (gears.table.join
       (awful.key [mod-key] "f"
                  #(do
                     (set $1.fullscreen (not $1.fullscreen))
                     ($1:raise))
                  {:description "toggle fullscreen"
                   :group "client"})

       (awful.key [mod-key shift] "space"
                  #($1:kill)
                  {:description "close window"
                   :group "client"})

       (awful.key [mod-key] "q" awful.client.floating.toggle
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
       ))

(set export.client-buttons
     [(awful.button [] 1
                    #($1:emit_signal "request::activate" "mouse_click" {:raise true}))
      (awful.button [mod-key] 1
                    #(do
                       ($1:emit_signal "request::activate" "mouse_click" {:raise true})
                       (awful.mouse.client.move $1)))
      (awful.button [mod-key shift] 1
                    #(do
                       ($1:emit_signal "request::activate" "mouse_click" {:raise true})
                       (awful.mouse.client.resize $1)))])

export
