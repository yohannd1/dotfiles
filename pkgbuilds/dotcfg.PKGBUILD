# Based off the furnace-git one in the AUR

pkgname=dotcfg
pkgver=a99c374
pkgrel=1
epoch=1
pkgdesc="Personal configuration daemon"
url="https://github.com/yohannd1/dotcfg"
depends=()
makedepends=('git' 'zig')
optdepends=()
provides=("$pkgname")
arch=('x86_64')
license=('GPL')
source=()
sha256sums=()

dir=${SOURCE_DIR:-$HOME/pj/code/dotcfg}

pkgver() {
  cd "$dir"
  git rev-parse --short HEAD
}

prepare() {
  :
}

build() {
  cd "$dir"
  zig build -Doptimize=ReleaseFast
}

package() {
  cd "$dir"
  mkdir -p "$pkgdir/usr/bin"
  cp "$dir/zig-out/bin/dotcfg" -t "$pkgdir/usr/bin"
}
