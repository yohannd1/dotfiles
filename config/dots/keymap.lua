return function(api)
  api.key("super ctrl shift c")
    :desc("start/restart compositor")
    :cond(api.is_xorg)
    :map("start-compositor")

  api.key("super enter")
    :desc("open terminal")
    :map("dotf.wrap.terminal")

  api.key("super shift enter")
    :desc("open terminal (float)")
    :map("dotf.wrap.terminal -c float")

  api.key("super alt g")
    :cond(api.is_xorg)
    :desc("xprop (result at home)")
    :map("xprop > ~/xprop_result.txt")

  api.key("super space")
    :desc("program launcher")
    :map("runnsend error-and-output fzrun")

  api.key("super alt o")
    :desc("power menu")
    :map("runnsend error-and-output fzpow")

  api.key("super c")
    :desc("screenshot (with selection)")
    :cond(api.is_xorg) -- TODO: use `dotf.screenshot` on wayland, idk
    :map("runnsend error ksnip -r")

  api.key("super shift c")
    :desc("screenshot (full screen)")
    :cond(api.is_xorg) -- TODO: use `dotf.screenshot` on wayland, idk
    :map("runnsend error ksnip -f")

  api.key("super F10")
    :desc("mount menu")
    :map("termup -f runread fzmount -m")

  api.key("super shift F10")
    :desc("unmount menu")
    :map("termup -f runread fzmount -u")

  api.key("super w")
    :desc("reload wallpaper")
    :map("setbg")

  api.key("super alt p")
    :desc("panic button")
    :cond(api.is_xorg)
    :map("xcalib -clear")

  api.key("super comma"):desc("decrease volume"):map("volumectl dec")
  api.key("super period"):desc("increase volume"):map("volumectl inc")
  api.key("super y"):desc("toggle mute"):map("volumectl togglemute")

  api.key("super alt comma"):desc("previous in player"):map("playerctl previous")
  api.key("super alt period"):desc("next in player"):map("playerctl next")
  api.key("super alt y"):desc("play/pause"):map("playerctl play-pause")

  api.key("super minus"):desc("decrease screen brightness"):map("backlightctl mod -10")
  api.key("super equal"):desc("increase screen brightness"):map("backlightctl mod +10")

  api.key("super F12")
    :desc("toggle red screen")
    :map("redscr toggle 90")

  api.key("super r")
    :desc("show report")
    :map("display-report")

  api.key("super b")
    :desc("open browser")
    :map("runnsend error $BROWSER")

  api.key("super e")
    :desc("open editor")
    :map("graphedit")

  api.key("super y")
    :desc("toggle tray")
    :map([[ pkill stalonetray || stalonetray -c "$XDG_CACHE_HOME/gen/stalonetrayrc" ]])

  api.key("super alt c")
    :desc("open calendar")
    :map("dotf.wrap.terminal -c float -g 65x10 -e runread cal -3")

  api.key("super alt m")
    :desc("open mixer")
    :map("dotf.wrap.terminal -c float -e pulsemixer")

  api.key("super alt t")
    :desc("open tmux tray")
    :map("dotf.wrap.terminal -c float -e tmux-tray")
end
