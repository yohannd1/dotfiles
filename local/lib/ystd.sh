## ystd: Yohanan's Shell Standard Library
## A collection of useful utilities for scripts.
## Might make them less protable, but makes the code less painful too.
## Code here should work with dash, bash and zsh.

# Check if a program exists
exists() {
  [ $# = 1 ] \
    && type "$1" >/dev/null 2>/dev/null
}

# Die if one of the dependencies in the arglist doesn't exist
depCheck() {
  local depArray=""
  for dep in "$@"; do
    type "$dep" >/dev/null 2>/dev/null \
      || local depArray="$depArray $dep"
  done
  if [ -n "$depArray" ]; then
    printf "Dependencies not found:%s\n" "$depArray"
    exit 1
  fi
}

# Set the terminal window title
setTitle() {
  [ $# != 1 ] && return 1
  printf '\033]0;%s\007' "$1"
}

# Source a file, if it exists
sourceIf() {
  [ $# != 1 ] && return 1
  [ -f "$1" ] \
    && source "$1"
}

# Source a file if it exists, or else shift and try again, until there's
# no arguments.
sourceAlt() {
  while [ $# != 0 ]; do
    sourceIf && return 0
    shift
  done
  return 1
}

isInteractive() { printf "%s" "$-" | grep -q '.*i.*'; }
isAndroid() { [ "$(uname -o)" = Android ]; }
isDisplay() { [ -n "$DISPLAY" ]; }
isTTY() { ! isDisplay && ! isAndroid; }
