# Useful functions
pathadd() {
  [ $# != 1 ] && return 1

  if [[ ":$PATH:" == *"$1"* ]]; then
    return 2
  else
    export PATH="${PATH:+"${PATH}:"}${1}"
  fi
}

# F_DIRS (specific to my dotfiles)
export F_PROJECTS="$HOME/projects"
export F_PERSIST="$HOME/persist"
export F_DOTFILES="$F_PROJECTS/dotfiles"
export F_WALLPAPERS="$F_PERSIST/wallpapers"
export F_USB_STATION="/mnt/usb-main"
if [ "$(uname -o)" = Android ]; then
  export F_TEMP="$HOME/.cache/tmp"
else
  export F_TEMP="/tmp"
fi

export EDITOR="nvim"
export TERMINAL="alacritty"
export BROWSER="qutebrowser"
export PAGER="less"
export MANPAGER="/bin/sh -c 'col -b | nvim -c PagerMode -'"
export DOTFILES="$F_DOTFILES" # Standard name for this one

export XDG_CONFIG_HOME="$HOME/.config"

export GOPATH="$HOME/.cache/go"
export CARGO_HOME="$HOME/.cache/cargo"
export RUSTUP_HOME="$HOME/.cache/rustup"
export ZDOTDIR="$HOME/.config/zsh"
export TASKRC="$HOME/.config/taskwarrior/taskrc"
export WINEPREFIX="$HOME/.cache/wine"

export FZF_DEFAULT_OPTS='--color=16 --height=60% --layout=reverse --border'
export BAT_THEME="base16"
export NNN_OPENER="nuke"

export KEYTIMEOUT=1
export TERM="xterm-256color"
export WM="bspwm"

pathadd "$HOME/.local/bin"
pathadd "$HOME/.cache/go"
pathadd "$CARGO_HOME/bin"
pathadd "$(ruby -e 'puts Gem.user_dir')/bin"

# Programs that I've installed in /opt
for dir in /opt/*; do
  pathadd "$dir/bin"
done
