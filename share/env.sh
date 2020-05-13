isAndroid() { test "$(uname -o)" = Android; }
pathadd() {
  [ $# != 1 ] && return 1

  if [[ ":$PATH:" == *"$1"* ]]; then # TODO: make this POSIX-compliant
    return 2
  else
    export PATH="${PATH:+"${PATH}:"}${1}"
  fi
}

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DOWNLOAD_DIR="$HOME/inbox"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_DIR="$HOME/.local/share"
export XDG_CURRENT_DESKTOP="none"

export STORAGE="$HOME/storage"
export WIKI="$HOME/wiki"
export PROJECTS="$HOME/projects"

export DOTFILES="$PROJECTS/dotfiles"
export EDITOR="nvim"
export TERMINAL="st"
export BROWSER="qutebrowser"
export PAGER="less"
export OPENER="openfork"
export READER="zathura"

export GOPATH="$XDG_CACHE_HOME/go"
export CARGO_HOME="$XDG_CACHE_HOME/cargo"
export RUSTUP_HOME="$XDG_CACHE_HOME/rustup"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export TASKRC="$XDG_CONFIG_HOME/taskwarrior/taskrc"
export INPUTRC="$XDG_CONFIG_HOME/inputrc"
export WINEPREFIX="$XDG_CACHE_HOME/wine" # TODO: move to DATA_DIR
export XAUTHORITY="$XDG_DATA_DIR/Xauthority" # might break some display managers, but I don't use them.
export LESSHISTFILE="-"

export LESS="-RC"
export FZF_DEFAULT_OPTS='--height=60% --layout=reverse --border'
export BAT_THEME="base16"
export NNN_OPENER="$OPENER"
export FLAMEDASH_SCR_FOLDER="$STORAGE/pictures/screenshots"
export SETBG_WALLPAPER_DIR="$STORAGE/pictures/wallpapers"
export BKMK_FILE="$WIKI/data/bookmarks"
export DOTSYNC_NO_BACKUP=1

export KEYTIMEOUT=1
export TERM="xterm-256color"
export WM="dwm"

export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# Dircolors
if [ -r ~/.local/share/dircolors ]; then
  eval "$(dircolors -b ~/.local/share/dircolors)"
fi

pathadd "$HOME/.local/bin"
pathadd "$GOPATH"
pathadd "$CARGO_HOME/bin"

# Seems to be slowing down, so I'll use the ony below and update when needed.
pathadd "${GEM_HOME:-$HOME/.gem}/ruby/2.7.0/bin"

# Programs that I've installed in /opt
for dir in /opt/*; do
  pathadd "$dir/bin"
done
