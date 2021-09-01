pathadd() {
  [ $# != 1 ] && return 1
  [ "$1" = "" ] && return

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
pathadd "$LUAROCKS_HOME/bin"
pathadd "$NPM_DIR/bin"

globpathadd "/opt"
globpathadd "${XDG_CACHE_HOME:-$HOME/.cache}/packs"

_luaPkgsAt() {
  printf "%s/?.lua;%s/?/init.lua" "$1" "$1"
}

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
