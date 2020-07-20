## A collection of useful utilities for scripts.
## Works with dash.

# check if a program exists
exists() { command -v "$1" >/dev/null 2>/dev/null; }

# error and die if one of the dependencies in the arglist doesn't exist
depCheck() {
  local depArray
  for dep in "$@"; do
    exists "$dep" || depArray="$depArray $dep"
  done
  if [ -n "$depArray" ]; then
    printf "Dependencies missing:%s\n" "$depArray"
    exit 1
  fi
}

# set the terminal window title
setTitle() { printf '\033]0;%s\007' "$1"; }

# source a file, if it exists
sourceIf() {
  [ $# != 1 ] && return 1
  [ -f "$1" ] && source "$1"
}

# check states
isInteractive() { printf "%s" "$-" | grep -q '.*i.*'; }
isAndroid() { test "$(uname -o)" = Android; }
isDisplay() { test -n "$DISPLAY"; }
isTTY() { ! isDisplay && ! isAndroid; }

# source a file if it exists, or else shift and try again, until there's no
# arguments.
sourceAlt() {
  while [ $# != 0 ]; do
    sourceIf "$1" && return 0
    shift
  done
  return 1
}
