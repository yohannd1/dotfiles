#!/usr/bin/env bash
#
# A script for bootstrapping my dotfiles installation.
# On early stages.
#
# Bashisms: arrays.

PROG=$(basename $0)

exists() {
  command -v "$1" >/dev/null 2>/dev/null;
}

usage() {
  printf >&2 "%s: %s\n" "$PROG" "installs dotfiles" # terrible description
  printf >&2 "Usage: %s\n" "$PROG <ACTION> <DEST>"
  printf >&2 "Where\n"
  printf >&2 "    ACTION : the action to be done.\n"
  printf >&2 "           | https: clone to DEST via HTTPS\n"
  printf >&2 "           | https-shallow: clone to DEST via HTTPS with --depth 1\n"
  printf >&2 "           | ssh: clone to DEST via SSH\n"
  printf >&2 "           | https-to-ssh: clone to (already existing, but https-based) DEST via SSH (thus converting it to SSH)\n"
  printf >&2 "    DEST   :  the path where the dotfiles will be installed."
  exit 1
}

[ $# != 2 ] && usage

case "$1" in
  https|https-shallow) DOTFILES_URL="https://github.com/yohannd1/dotfiles" ;;
  ssh|https-to-ssh) DOTFILES_URL="git@github.com:yohannd1/dotfiles" ;;
  *)
    printf >&2 "Invalid ACTION: %s\n" "$1"
    printf >&2 "\n"
    usage
    ;;
esac

if [ "$1" != https-to-ssh ] && [ -e "$2" ]; then
  printf >&2 "DEST %s already exists. Please delete it or choose another path.\n" "$2"
  exit 1
fi

# TODO: get the similar thing from LARBS or something and put it here instead
for dependency in git python3; do
  if ! exists "$dependency"; then
    printf >&2 "Could not find %s in PATH - attempting to install it...\n" "$dependency"
    if exists pacman; then
      sudo pacman -Syy "$dependency"
    elif exists apt; then
      sudo apt install "$dependency"
    elif exists apt-get; then
      sudo apt-get install "$dependency"
    else
      printf >&2 "Could not find your package manager. Please install said dependency manually.\n"
      exit 1
    fi
  fi
done

case "$1" in
  https-to-ssh)
    if [ ! -d "$2" ]; then
      printf >&2 "%s is not a directory.\n" "$2"
      exit 1
    fi
    temp=$(mktemp) \
      && rm -f "$temp" \
      && mv "$2" "$temp" || exit 1
    if git clone "$DOTFILES_URL" "$2"; then
      rm -rf "$temp"
    else
      mv "$temp" "$2"
    fi
    ;;
  *)
    [ "$1" = https-shallow ] && shallow=(--depth 1) || shallow=()
    git clone "${shallow[@]}" "$DOTFILES_URL" "$2" \
      && mkdir -p ~/.local/share/dots \
      && echo "$(realpath -m "$2")" > ~/.local/share/dots/dotpath \
      && echo "gruvbox-dark-medium" > ~/.local/share/dots/theme \
      && {
        export DOTFILES="$2"
        . "$DOTFILES/config/dots/env.sh"
        . "$DOTFILES/config/dots/path.sh"
        sysm
      }
    ;;
esac
