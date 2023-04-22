pathadd() {
  [ $# != 1 ] && return 1
  [ "$1" = "" ] && return

  # I used to have a "normalization" thing here to replace '/' with '\/' but it seems grep doesn't care about that anymore? Or maybe it never did.

  # Check if the path's there
  if printf "%s" ":$PATH:" | grep -vq ":${1}:"; then
    if printf "%s" "$PATH" | grep -vq ':$'; then
      export PATH="${PATH}:"
    fi

    export PATH="${PATH}${1}"
  fi
}

globpathadd() {
  [ -d "$1" ] || return 1

  for pack in "$1"/*; do
    pack_path=$(realpath -m "$pack/bin")
    pathadd "$pack_path"
  done
}

pathadd ~/.local/bin
[ "$DOTFILES" ] && pathadd "$DOTFILES/scripts"
pathadd ~/storage/scripts
[ "$GOPATH" ] && pathadd "$GOPATH"
[ "$CARGO_HOME" ] && pathadd "$CARGO_HOME/bin"
pathadd "${GEM_HOME:-$HOME/.gem}/bin"
pathadd ~/.nimble/bin
[ "$NPM_DIR" ] && pathadd "$NPM_DIR/bin"

globpathadd "/opt"
globpathadd "${XDG_CACHE_HOME:-$HOME/.cache}/packs"

if [ "$LUAROCKS_HOME" ]; then
  _luaPkgsAt() {
    printf "%s/?.lua;%s/?/init.lua" "$1" "$1"
  }

  pathadd "$LUAROCKS_HOME/bin"

  # Paths for lua 5.2 and on
  for luaVer in 2 3 4; do
    shareDir="$LUAROCKS_HOME/share/lua/5.${luaVer}"
    libDir="$LUAROCKS_HOME/lib/lua/5.${luaVer}"
    dotfLibDir="$DOTFILES/lib"

    eval "export LUA_PATH_5_${luaVer}='$(_luaPkgsAt "$shareDir");$(_luaPkgsAt "$libDir");$(_luaPkgsAt "$dotfLibDir");;'"
    eval "export LUA_CPATH_5_${luaVer}='$libDir/loadall.so;$libDir/?.so;;'"
  done

  # Paths for lua 5.1
  libDir="$LUAROCKS_HOME/lib/lua/5.1"
  eval "export LUA_PATH='$(_luaPkgsAt "$LUAROCKS_HOME/share/lua/5.1");$(_luaPkgsAt "$libDir");$(_luaPkgsAt "$DOTFILES/lib");;'"
  eval "export LUA_CPATH='$libDir/loadall.so;$libDir/?.so;;'"
fi
