pathadd() {
  [ $# != 1 ] && return 1

  # normalize path
  _normalized="$(printf "%s" "$1" | sed 's / \\/ g')"
  printf "%s" ":$PATH:" | grep -vq :"$_normalized": && {
    printf "%s" "$PATH" | grep -vq ':$' && export PATH="${PATH}:"
    export PATH="${PATH}${1}"
  }
}

globpathadd() {
  [ -d "$1" ] && pushd "$1" >/dev/null 2>/dev/null || return 1
  fd -td -d1 | while read pack; do
    pathadd "$1/$pack/bin"
  done
  popd >/dev/null 2>/dev/null
}

pathadd ~/.local/bin
pathadd "$DOTFILES/scripts"
pathadd ~/storage/scripts
pathadd "$GOPATH"
pathadd "$CARGO_HOME/bin"
pathadd "${GEM_HOME:-$HOME/.gem}/ruby/2.7.0/bin"
pathadd ~/.nimble/bin
pathadd ~/.luarocks/bin

globpathadd "/opt"
globpathadd "${XDG_CACHE_HOME:-$HOME/.cache}/packs"
