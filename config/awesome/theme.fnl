(fn xgetres [res-name error-on-nothing]
  (local error-on-nothing
    (if (= error-on-nothing nil) true error-on-nothing))

  (local proc-fd (-> "xgetres %s"
                     (string.format res-name)
                     (assert "failed to open xgetres command")
                     (io.popen "r")))

  (var output (proc-fd:read "*a"))
  (proc-fd:close)
  (assert output "failed to read output from command")

  (set output (-> output
                  (string.gsub "[\n\r]+" " ")
                  (string.gsub "^%s+" "")
                  (string.gsub "%s+$" "")))

  (match output
    "" (if error-on-nothing
         (error (.. "Resource unavailable: " res-name))
         nil)
    _ output))

(local beautiful (require :beautiful))
(local dpi beautiful.xresources.apply_dpi)
(local assets beautiful.theme_assets)
(local gfs (require :gears.filesystem))

(local theme-path (gfs.get_themes_dir))
(local taglist-square-size (dpi 4))

(local theme {})

;; font
(set theme.font (xgetres :awesome.font))

;; foreground colors
(set theme.fg_normal (xgetres :awesome.color5))
(set theme.fg_focus (xgetres :awesome.color5))
(set theme.fg_urgent (xgetres :awesome.color5))
(set theme.fg_minimize (xgetres :awesome.color5))

;; background colors
(set theme.bg_normal (xgetres :awesome.color0))
(set theme.bg_focus (xgetres :awesome.color2))
(set theme.bg_urgent (xgetres :awesome.color8))
(set theme.bg_minimize (xgetres :awesome.color3))
(set theme.bg_systray (xgetres :awesome.color0))

;; border colors
(set theme.border_normal (xgetres :awesome.border-normal))
(set theme.border_focus (xgetres :awesome.border-focus))
(set theme.border_marked (xgetres :awesome.border-marked))

;; numeric attributes
(set theme.border_width (dpi 1))
(set theme.useless_gap (dpi 0))

;; icon theme (default: /usr/share/icons & /user/share/hicolor)
(set theme.icon_theme nil)

;; There are other variable sets overriding the default one when defined, the sets are:
;; taglist_[bg|fg]_[focus|urgent|occupied|empty|volatile]
;; tasklist_[bg|fg]_[focus|urgent]
;; titlebar_[bg|fg]_[normal|focus]
;; tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
;; mouse_finder_[color|timeout|animate_timeout|radius|factor]
;; prompt_[fg|bg|fg_cursor|bg_cursor|font]
;; hotkeys_[bg|fg|border_width|border_color|shape|opacity|modifiers_fg|label_bg|label_fg|group_margin|font|description_font]
(set theme.taglist_squares_sel
     (assets.taglist_squares_sel taglist-square-size theme.fg_normal))
(set theme.taglist_squares_unsel
     (assets.taglist_squares_unsel taglist-square-size theme.fg_normal))

;; Variables set for theming the menu:
;; menu_[bg|fg]_[normal|focus]
;; menu_[border_color|border_width]
(set theme.menu_submenu_icon (.. theme-path "default/submenu.png"))
(set theme.menu_height (dpi 15))
(set theme.menu_width  (dpi 100))

;; layout icons
(set theme.layout_fairh      (.. theme-path "default/layouts/fairhw.png"))
(set theme.layout_fairv      (.. theme-path "default/layouts/fairvw.png"))
(set theme.layout_floating   (.. theme-path "default/layouts/floatingw.png"))
(set theme.layout_magnifier  (.. theme-path "default/layouts/magnifierw.png"))
(set theme.layout_max        (.. theme-path "default/layouts/maxw.png"))
(set theme.layout_fullscreen (.. theme-path "default/layouts/fullscreenw.png"))
(set theme.layout_tilebottom (.. theme-path "default/layouts/tilebottomw.png"))
(set theme.layout_tileleft   (.. theme-path "default/layouts/tileleftw.png"))
(set theme.layout_tile       (.. theme-path "default/layouts/tilew.png"))
(set theme.layout_tiletop    (.. theme-path "default/layouts/tiletopw.png"))
(set theme.layout_spiral     (.. theme-path "default/layouts/spiralw.png"))
(set theme.layout_dwindle    (.. theme-path "default/layouts/dwindlew.png"))
(set theme.layout_cornernw   (.. theme-path "default/layouts/cornernww.png"))
(set theme.layout_cornerne   (.. theme-path "default/layouts/cornernew.png"))
(set theme.layout_cornersw   (.. theme-path "default/layouts/cornersww.png"))
(set theme.layout_cornerse   (.. theme-path "default/layouts/cornersew.png"))

;; Variable set for theming notifications:
;; notification_font
;; notification_[bg|fg]
;; notification_[width|height|margin]
;; notification_[border_color|border_width|shape|opacity]
;; TODO

;; generate awesome icon
(set theme.awesome_icon
     (assets.awesome_icon theme.menu_height theme.bg_focus theme.fg_focus))

theme
