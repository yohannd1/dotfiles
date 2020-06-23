# XDG dirs
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DOWNLOAD_DIR="$HOME/inbox"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_DIR="$HOME/.local/share"
export XDG_CURRENT_DESKTOP="none"

# personal dirs
export STORAGE="$HOME/storage"
export WIKI="$HOME/wiki"
export PROJECTS="$HOME/projects"
export DOTFILES="$PROJECTS/dotfiles"

# global options
export WM="dwm"
export TERM="xterm-256color"
export EDITOR="nvim"
export TERMINAL="st"
export BROWSER="qutebrowser"
export TERMBROWSER="w3m"
export PAGER="less"
export OPENER="openfork"
export READER="zathura"
export THEME="tomorrow-night"

# configuration files/folders
export GOPATH="$XDG_CACHE_HOME/go"
export CARGO_HOME="$XDG_CACHE_HOME/cargo"
export RUSTUP_HOME="$XDG_CACHE_HOME/rustup"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export TASKRC="$XDG_CONFIG_HOME/taskwarrior/taskrc"
export INPUTRC="$XDG_CONFIG_HOME/inputrc"
export WINEPREFIX="$XDG_DATA_DIR/wine"
export XAUTHORITY="$XDG_DATA_DIR/Xauthority" # might break some display managers, but I don't use them.
export LESSHISTFILE="-"

# program options
export LESS="-RC"
export KEYTIMEOUT=1
export NNN_OPENER="$OPENER"
export NNN_TRASH=1
export FZF_DEFAULT_OPTS='
  --height=80% --layout=reverse --border
  --color fg:4,fg+:5
  --color hl:6,hl+:7
  --color prompt:8,marker:5,pointer:8
  --color spinner:3,gutter:1,info:3
'
export GCC_COLORS='error=01;38;5;8:warning=01;38;5;9:note=01;38;5;12:caret=01;32:locus=01;38;5;11:quote=03'

# dotfiles program options
export DIR_BOOKMARKS="$STORAGE/share/dir-bookmarks"
export FLAMEW_SCR_FOLDER="$STORAGE/pictures/screenshots"
export SETBG_WALLPAPER_DIR="$STORAGE/pictures/wallpapers"
export SETBG_THEME="storm"
export BKMK_FILE="$WIKI/data/bookmarks"
export DOTSYNC_NO_BACKUP=1

# dircolors
if [ -r ~/.config/dircolors ]; then
  eval "$(dircolors -b ~/.config/dircolors)"
fi
