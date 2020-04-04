pathadd() {
  [ $# != 1 ] && return 1

  if [[ ":$PATH:" == *"$1"* ]]; then
    return 2
  else
    export PATH="${PATH:+"${PATH}:"}${1}"
  fi
}

# F_DIRS (specific to my dotfiles)
export F_WIKI="$HOME/wiki"
export F_PROJECTS="$HOME/projects"
export F_PERSIST="$HOME/persist"
export F_DOTFILES="$F_PROJECTS/dotfiles"
export F_WALLPAPERS="$F_PERSIST/wallpapers"
export F_USB_STATION="/mnt/usb-main"
export F_ARTICLES="$F_PERSIST/docs/articles"
if [ "$(uname -o)" = Android ]; then
  export F_TEMP="$HOME/.cache/tmp"
else
  export F_TEMP="/tmp"
fi

export EDITOR="nvim"
export TERMINAL="st"
export BROWSER="qutebrowser"
export PAGER="less"
export DOTFILES="$F_DOTFILES"
export OPENER="openfork"

export XDG_CONFIG_HOME="$HOME/.config"

export GOPATH="$HOME/.cache/go"
export CARGO_HOME="$HOME/.cache/cargo"
export RUSTUP_HOME="$HOME/.cache/rustup"
export ZDOTDIR="$HOME/.config/zsh"
export TASKRC="$HOME/.config/taskwarrior/taskrc"
export WINEPREFIX="$HOME/.cache/wine"

export FZF_DEFAULT_OPTS='--height=60% --layout=reverse --border'
export BAT_THEME="base16"
export NNN_OPENER="$OPENER"
export FLAMEDASH_SCR_FOLDER="$HOME/photos/screenshots"
export BKMK_FILE="$F_WIKI/data/bookmarks"

export KEYTIMEOUT=1
export TERM="xterm-256color"
export WM="bspwm"

export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# Dircolors
if [ -r ~/.local/share/dircolors ]; then
  eval "$(dircolors -b ~/.local/share/dircolors)"
fi

pathadd "$HOME/.local/bin"
pathadd "$HOME/.cache/go"
pathadd "$CARGO_HOME/bin"

# pathadd "$(ruby -e 'puts Gem.user_dir')/bin" # Seems to be slowing down, so I'll use the ony below and update when needed.
pathadd "$HOME/.gem/ruby/2.7.0"

# Programs that I've installed in /opt
for dir in /opt/*; do
  pathadd "$dir/bin"
done
