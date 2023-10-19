# Based off the furnace-git one in the AUR

pkgname=furnace-git
_truepkg=furnace
pkgver=dev152.r1640.gecd28b5e
pkgrel=1
epoch=1
pkgdesc="A multi-system chiptune tracker compatible with DefleMask modules"
url="https://github.com/tildearrow/furnace"
depends=('sdl2' 'libsndfile' 'fmt' 'hicolor-icon-theme' 'alsa-lib' 'fftw' 'rtmidi')
makedepends=('git' 'jack' 'cmake')
optdepends=('jack: JACK audio support')
provides=("$_truepkg")
arch=('x86_64')
license=('GPL')
source=()
sha256sums=()

dir="$HOME/pj/code/furnace-fork"

_dlog() {
  printf >&2 "[at %s] " "$PWD"
  printf >&2 -- "$@"
  printf >&2 "\n"
}

pkgver() {
  cd "$dir"
  # git co yohanan >/dev/null
  git describe --long --tags | sed 's/\([^-]*-g\)/r\1/;s/-/./g'
}

prepare() {
  :
}

build() {
  _dlog "BUILDING"

  mkdir -p build
  cd build
  if [ ! -f Makefile ]; then
    _dlog "Makefile not found - running cmake..."
    cmake -DCMAKE_INSTALL_PREFIX=/usr -DBUILD_GUI=ON -DSYSTEM_FFTW=ON -DSYSTEM_FMT=OFF -DSYSTEM_ZLIB=ON -DSYSTEM_LIBSNDFILE=ON -DSYSTEM_SDL2=ON -DSYSTEM_RTMIDI=ON -DWITH_JACK=ON "$dir"
  else
    _dlog "Makefile found! Skipping to build"
  fi

  cmake --build . -j$(nproc)
}

package() {
  _dlog "PACKAGING"

  DESTDIR="$pkgdir" cmake --install build
  cd "$dir"

  install -Dm644 -T res/icon.iconset/icon_16x16.png "$pkgdir/usr/share/icons/hicolor/16x16/apps/${_truepkg}.png"
  install -Dm644 -T res/icon.iconset/icon_32x32.png "$pkgdir/usr/share/icons/hicolor/32x32/apps/${_truepkg}.png"
  install -Dm644 -T res/icon.iconset/icon_64x64.png "$pkgdir/usr/share/icons/hicolor/64x64/apps/${_truepkg}.png"
  install -Dm644 -T res/icon.iconset/icon_128x128.png "$pkgdir/usr/share/icons/hicolor/128x128/apps/${_truepkg}.png"
  install -Dm644 -T res/icon.iconset/icon_256x256.png "$pkgdir/usr/share/icons/hicolor/256x256/apps/${_truepkg}.png"
  install -Dm644 -T res/icon.iconset/icon_512x512.png "$pkgdir/usr/share/icons/hicolor/512x512/apps/${_truepkg}.png"
}
