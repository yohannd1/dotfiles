_exists() { command -v "$1" >/dev/null 2>/dev/null; }
_isAndroid() { test -d ~/.termux; } # miserable way to detect termux

# dotfiles dir
_dotpath=~/.local/share/dots/dotpath
_fallback_dotpath=~/.dotfiles
if [ -f "$_dotpath" ]; then
  export DOTFILES=$(cat "$_dotpath")
else
  printf >&2 "warning: %s doesn't exist - %s will fallback to %s" "$_dotpath" '$DOTFILES' "$_fallback_dotpath"
  export DOTFILES="$_fallback_dotpath"
fi

# wayland stuff
if [ "$WAYLAND_DISPLAY" ]; then
  # https://mastransky.wordpress.com/2020/03/16/wayland-x11-how-to-run-firefox-in-mixed-environment/
  # https://wiki.archlinux.org/title/Firefox#Wayland

  export MOZ_DBUS_REMOTE=1
  export MOZ_ENABLE_WAYLAND=1
  export SDL_VIDEODRIVER=wayland
  export _JAVA_AWT_WM_NONREPARENTING=1
  export QT_QPA_PLATFORM=wayland
  export GDK_BACKEND=wayland
fi

# the laziest way to force the locale I want
export LC_ALL='en_US.UTF-8'

# XDG dirs
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DOWNLOAD_DIR="$HOME/inbox"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_DIR="$HOME/.local/share"
export XDG_DATA_HOME="$XDG_DATA_DIR"

# personal dirs (should this even be here lol)
export ACR_WIKI_DIR="$HOME/wiki/vimwiki"

# global options
if _isAndroid; then
  export EDITOR=nvim # not sure yet but nnn struggles with dotf.wrap.editor. Probably crap android exec stuff...
else
  export EDITOR=dotf.wrap.editor
fi
export TERMUX_SHELL=zsh

export WAYLAND_TERMINAL=foot
export XORG_TERMINAL=st
export TERMINAL=dotf.wrap.terminal

export BROWSER=librewolf
_isAndroid && export BROWSER=termux-open-url

export TERMBROWSER=w3m
export PAGER=less
export OPENER=openfork
export READER=zathura
export FILEMAN=nnn

# SDKs and programming tools
export GOPATH="$XDG_CACHE_HOME/go"
export CARGO_HOME="$XDG_CACHE_HOME/cargo"
export VCPGK_ROOT="$XDG_DATA_DIR/vcpkg"
export RUSTUP_HOME="$XDG_CACHE_HOME/rustup"
export GEM_HOME="$XDG_DATA_DIR/gem"
export GEM_SPEC_CACHE="$XDG_CACHE_HOME/gem"
export GRADLE_USER_HOME="$XDG_DATA_HOME/gradle"
export ZIGUP_INSTALL_DIR="$XDG_CACHE_HOME/zigup"
export ZIGUP_PATH_LINK="$HOME/.local/bin/zig"
export LUAROCKS_HOME="$HOME/.luarocks"
export JANET_MODPATH="$XDG_CACHE_HOME/janet"
export JANET_BINPATH="$HOME/.local/bin"
export ZSH_PLUGIN_PATH="$XDG_CACHE_HOME/zsh-plugins"
export R_PROFILE_USER="$DOTFILES/config/Rprofile.R"
export R_LIBS_USER="$XDG_CACHE_HOME/rlibs";
if _exists sccache; then
  export RUSTC_WRAPPER=sccache
fi
if _exists ccache; then
  export CMAKE_CXX_COMPILER_LAUNCHER=ccache
  export CMAKE_C_COMPILER_LAUNCHER=ccache
fi

# configuration files/folders
export _ZL_DATA="$XDG_DATA_DIR/zlua"
export _ZL_FZF_HEIGHT="15" # no height limit!
export WINEPREFIX="$XDG_DATA_DIR/wine32"
export WINEW_32_PREFIX="$XDG_DATA_DIR/wine32"
export WINEW_64_PREFIX="$XDG_DATA_DIR/wine64"
export _JAVA_OPTIONS="-Djava.util.prefs.userRoot='$XDG_CONFIG_HOME/java' -Dawt.useSystemAAFontSettings=on -Dswing.aatext=true"
export JAVA_FONTS="/usr/share/fonts/TTF"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export TASKRC="$XDG_CONFIG_HOME/taskwarrior/taskrc"
export TIMEWARRIORDB="$WIKI/data/timewarrior"
export INPUTRC="$XDG_CONFIG_HOME/inputrc"
export XAUTHORITY="$XDG_DATA_DIR/Xauthority" # might break some display managers
export LESSKEY="$XDG_CONFIG_HOME/less/lesskey"
export LESSHISTFILE="$XDG_CACHE_HOME/less/history"
export WGETRC="$XDG_CONFIG_HOME/wgetrc"
export IRBRC="$XDG_CONFIG_HOME/irb/irbrc"
export FZRUN_PACKAGES_FOLDER="$HOME/storage/software/packages"

# xorg & wayland
export KBRATE_DELAY="250"
export KBRATE_INTERVAL="45"

# program options
export GIT_EDITOR="$EDITOR"
export LESS="-RC"
export KEYTIMEOUT=1
export FZF_DEFAULT_OPTS='
  --layout=reverse --no-border
  --color fg:5,fg+:7
  --color hl:6,hl+:4
  --color prompt:8,marker:5,pointer:8
  --color spinner:3,gutter:1,info:3'
export GREP_COLORS='ms=01;34:mc=01;34:sl=:cx=:fn=35:ln=32:bn=32:se=36'
export OPEN_ALT=dotf.unknown-filetype
export OPEN_FALLBACK_SILENT=1
_isAndroid || export GTK_THEME="Adwaita-dark"
export QT_STYLE_OVERRIDE="Adwaita-Dark"
export PYTHON_BASIC_REPL=1 # weird thing that happened but i dont care that much tbh - https://github.com/python/cpython/issues/118840

# nnn config
{
  export NNN_OPENER="$OPENER"
  export NNN_TRASH=1

  export NNN_OPTS="A"
  if _exists cpg; then
    export NNN_OPTS="${NNN_OPTS}r"
  fi

  # FIXME: not working fully... I'm confused.
  _BLK="0B" _CHR="0B" _DIR="04" _EXE="06" _REG="00" _HARDLINK="06" _SYMLINK="06" _MISSING="00" _ORPHAN="09" _FIFO="06" _SOCK="0B" _OTHER="06"
  export NNN_FCOLORS="$_BLK$_CHR$_DIR$_EXE$_REG$_HARDLINK$_SYMLINK$_MISSING$_ORPHAN$_FIFO$_SOCK$_OTHER"
}

# Pinentry User Data
_pue="tty"
[ "$DISPLAY" ] && _pue="xorg"
[ "$WAYLAND_DISPLAY" ] && _pue="wayland"
export PINENTRY_USER_DATA=$(printf '{"context":"%s","dotcfg_socket":"%s"}' "$_pue" "$DOTCFG_SOCKET")
unset _pue

export GPG_TTY=$(tty)

_gcc_colors='error    = 01;38;5;8
             :warning = 01;38;5;9
             :note    = 01;38;5;12
             :caret   = 01;32
             :locus   = 01;38;5;4
             :quote   = 03'
export GCC_COLORS=$(printf "%s" "$_gcc_colors" | tr -d ' ' | tr -d '\n')

# dotfiles program options
export DIR_BOOKMARKS=~/storage/local/share/bookmarks.sh
export FLAMEW_SCR_FOLDER=~/storage/pictures/screenshots
export SETBG_WALLPAPER_TYPE="image"
export BKMK_FILE="$WIKI/data/bookmarks.json"
export ITMN_FILE="$WIKI/data/itmn.json"
export RIPGREP_CONFIG_PATH="$XDG_CONFIG_HOME/ripgreprc"
export GUILE_LOAD_PATH="$DOTFILES/lib/guile"
export CLANG_FORMAT_C_CONFIG="$DOTFILES/config/clang-format-c.yaml"
export CLANG_FORMAT_CPP_CONFIG="$DOTFILES/config/clang-format-cpp.yaml"
export X_COMPOSITOR="picom"
export FILEHIST_MAX_LIMIT=500
export FILEHIST_NO_TIDY=1
export EXT_DEFAULT_MODE="new"
export NIRI_CONFIG="$XDG_CACHE_HOME/gen/niri.kdl"

# prevent annoying timeouts on some GTK applications.
# FIXME: is there any downside to this?
# FIXME: IS THIS EVEN WORKING????????????
# export GTK_USE_PORTAL=0

# Input Method
# export GTK_IM_MODULE=fcitx
# export QT_IM_MODULE=fcitx
# export SDL_IM_MODULE=fcitx
# export XMODIFIERS='@im=fcitx'

# because for some reason carla didn't add the /etc/ld.so.conf entry on my system
export LD_LIBRARY_PATH='/usr/lib/carla'

# dircolors
if [ -r ~/.config/dircolors ]; then
  eval "$(dircolors -b ~/.config/dircolors)"
fi

# system-specific config
RESLUA_ENABLE_LIGATURES=false
case "$HOST" in
  core)
    export USE_BUILTIN_1080P=
    export RESLUA_FONT_SIZE=1.25
    export RESLUA_FONT_NAME="CascadiaCode"
    ;;
  core2)
    export RESLUA_FONT_SIZE=1.35
    export RESLUA_FONT_NAME="Iosevka"
    export QT_SCALE_FACTOR=1.15
    export VOLUMECTL_INCREMENT=5
    ;;
esac
