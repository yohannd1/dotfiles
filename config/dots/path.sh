pathadd() {
  [ $# != 1 ] && return 1

  # normalize path
  _normalized="$(printf "%s" "$1" | sed 's / \\/ g')"
  printf "%s" ":$PATH:" | grep -vq :"$_normalized": && {
    printf "%s" "$PATH" | grep -vq ':$' && export PATH="${PATH}:"
    export PATH="${PATH}${1}"
  }
}

pathadd "$HOME/.local/bin"
pathadd "$GOPATH"
pathadd "$CARGO_HOME/bin"

# Seems to be slowing down, so I'll use the ony below and update when needed.
pathadd "${GEM_HOME:-$HOME/.gem}/ruby/2.7.0/bin"

# Programs installed in /opt
for dir in /opt/*; do
  pathadd "$dir/bin"
done
