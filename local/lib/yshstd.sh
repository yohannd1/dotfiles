## yshstd: Yohanan's Shell Standard Library
## A collection of useful utilities for scripts.
## Might make them less protable, but makes the code less painful too.
## Code here should work with dash, bash and zsh.

depCheck() {
  local depArray=""
  for dep in "$@"; do
    ! type "$dep" >/dev/null 2>/dev/null \
      && local depArray="$depArray $dep"
  done
  if [ -n "$depArray" ]; then
    printf "Dependencies not found:%s\n" "$depArray"
    exit 1
  fi
}
