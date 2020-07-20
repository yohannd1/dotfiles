## Filematch library
## TODO: description
## Needed features: local functions

# Clean up some variables
_filematch_initialized=
_filematch_file=

## (Internal function)
## Used by external functions for checking if filematch has been initialized with a file.
## Usage: <PROGNAME>
_filematch_guard() {
  if [ -z "$_filematch_initialized" ]; then
    printf >&2 "Attempted to use %s without initializing filematch" "$1"
    exit 127
  fi
}

## (Internal function)
## Usage: <PATTERN> <DATA>
_filematch_casematch() {
  local pattern=$(printf "%s" "$1" | sed 's \* .* g;s \. . g;s | \\| g;s \(.*\) ^\\(\1\\)$ g')
  local data=$2

  printf "%s" "$data" | grep -q "$pattern"
}

## Initializes filematch with the file FILE.
## Usage: <FILE>
filematch_initialize() {
  if [ $# = 1 ]; then
    _filematch_file="$1"
    _filematch_filename="$(basename "$_filematch_file")"
    _filematch_filetype="$(file --dereference --brief "$_filematch_file")"
    _filematch_mimetype="$(file --dereference --brief --mime-type "$_filematch_file")"
    _filematch_ext="${_filematch_filename##*.}"
    if [ "$_filematch_ext" ]; then
      _filematch_ext=$(printf "%s" "$_filematch_ext" | tr '[:upper:]' '[:lower:]')
    fi
    if printf "%s" "$_filematch_file" | grep -q '^\(\w*\)://'; then
      _filematch_urlprefix=${_filematch_file%://*}
    fi

    if [ -z "$_filematch_urlprefix" ] && [ ! -e "$_filematch_file" ]; then
      msg=$(printf 'Could not analyze "%s": no such file, directory or url' "$_filematch_file")
      if [ -t 2 ]; then
        printf >&2 "%s\n" "$msg"
      else
        notify-send -- "$msg"
      fi
      exit 1
    fi

    _filematch_initialized=1
  else
    printf >&2 "Usage: %s\n" "$(basename $0) <FILE>"
    return 1
  fi
}

## Matches the file info against PAT. The info used is specified by PTYPE.
## Returns an exit code indicating whether it matched or not.
## Usage: <PTYPE> <PAT>
##        where PTYPE := mime ext url
match() {
  _filematch_guard "$(basename $0)"
  local ptype="$1" pat="$2"
  shift 2

  case "$ptype" in
    mime) [ -z "$_filematch_urlprefix" ] && _filematch_casematch "$pat" "$_filematch_mimetype" ;;
    ext) [ -z "$_filematch_urlprefix" ] && _filematch_casematch "$pat" "$_filematch_ext" ;;
    url) [ "$_filematch_urlprefix" ] && _filematch_casematch "$pat" "$_filematch_urlprefix" ;;
    *)
      printf >&2 "Invalid pattern type: %s\n" "$ptype"
      exit 1
      ;;
  esac
}
