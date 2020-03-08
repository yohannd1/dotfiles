#!/usr/bin/env sh
# I think this file is better sourced.

export PROJECTS="${HOME}/projects"
export DOTFILES="${PROJECTS}/dotfiles"

export EDITOR="nvim"
export TERMINAL="alacritty"
export BROWSER="qutebrowser"
export PAGER="less"
export MANPAGER="/bin/sh -c 'col -b | nvim -c PagerMode -'"

export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_DOWNLOAD_DIR="$HOME/inbox"

export GOPATH="${HOME}/.cache/go"
export CARGO_HOME="${HOME}/.cache/cargo"
export RUSTUP_HOME="${HOME}/.cache/rustup"
export ZDOTDIR="${HOME}/.config/zsh"
export TASKRC="${HOME}/.config/taskwarrior/taskrc"
export WINEPREFIX="${HOME}/.cache/wine"

export FZF_DEFAULT_OPTS='--color=16 --height 60% --layout=reverse --border'
export BAT_THEME="base16"

export KEYTIMEOUT=1
export TERM="xterm-256color"
export WM="i3"
