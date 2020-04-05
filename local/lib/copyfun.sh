# This function can be used to copy functions to other names.
# Based off somebody's answer at https://stackoverflow.com/questions/1203583/how-do-i-rename-a-bash-function

copyfun() {
  if [ $# != 2 ]; then
    echo "Invalid amount of arguments."
  fi
  declare -F "$1" >/dev/null || exit 1;
  eval "$(printf "%s()\n" "$2"; declare -f "$1" | tail -n +2)"
}
