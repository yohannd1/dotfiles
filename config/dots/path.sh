#!/usr/bin/env sh

_pathsh_glob() {
  [ -d "$1" ] || return 1
  find "$(realpath "$1")" -maxdepth 1 -type d
}

# this removes duplicates while still mantaining order
_pathsh_noDup() { awk '!x[$0]++'; }

_pathsh_printAll() {
  echo ~/.local/bin
  echo ~/.nix-profile/bin
  [ "$DOTFILES" ] && echo "$DOTFILES/scripts"
  echo ~/storage/local/scripts
  [ "$GOPATH" ] && echo "$GOPATH"
  [ "$CARGO_HOME" ] && echo "$CARGO_HOME/bin"
  echo "${GEM_HOME:-$HOME/.gem}/bin"
  echo ~/.nimble/bin
  [ "$NPM_DIR" ] && echo "$NPM_DIR/bin"
  echo "$XDG_CONFIG_HOME/composer/vendor/bin"
  [ "$LUAROCKS_HOME" ] && echo "$LUAROCKS_HOME/bin"

  _pathsh_glob "/opt"
  _pathsh_glob "${XDG_CACHE_HOME:-$HOME/.cache}/packs"

  # current values in PATH
  printf "%s\n" "$PATH" | tr ':' '\n'
}

_pathsh_path=$(_pathsh_printAll) && {
  PATH=$(printf "%s" "$_pathsh_path" | _pathsh_noDup | tr '\n' ':')
  export PATH
}

if [ "$LUAROCKS_HOME" ]; then
  _luaPkgsAt() { printf "%s/?.lua;%s/?/init.lua" "$1" "$1"; }

  dotfLibDir="$DOTFILES/lib/lua"

  # Paths for lua 5.2 and on
  for luaVer in 2 3 4; do
    shareDir="$LUAROCKS_HOME/share/lua/5.${luaVer}"
    libDir="$LUAROCKS_HOME/lib/lua/5.${luaVer}"
    lrDir="$LUAROCKS_HOME/lib/luarocks/5.${luaVer}"

    eval "export LUA_PATH_5_${luaVer}='$(_luaPkgsAt "$shareDir");$(_luaPkgsAt "$libDir");$(_luaPkgsAt "$dotfLibDir");$(_luaPkgsAt "$lrDir");;'"
    eval "export LUA_CPATH_5_${luaVer}='$libDir/loadall.so;$libDir/?.so;;'"
  done

  # Paths for lua 5.1
  libDir="$LUAROCKS_HOME/lib/lua/5.1"
  eval "export LUA_PATH='$(_luaPkgsAt "$LUAROCKS_HOME/share/lua/5.1");$(_luaPkgsAt "$libDir");$(_luaPkgsAt "$dotfLibDir");;'"
  eval "export LUA_CPATH='$libDir/loadall.so;$libDir/?.so;;'"
fi
