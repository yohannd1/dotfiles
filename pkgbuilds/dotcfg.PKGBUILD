# Based off the furnace-git one in the AUR

pkgname=dotcfg
pkgver=7c9a493
pkgrel=1
epoch=1
pkgdesc="Personal configuration daemon"
url="https://github.com/YohananDiamond/dotcfg"
depends=()
makedepends=('git' 'zig')
optdepends=()
provides=("$pkgname")
arch=('x86_64')
license=('GPL')
source=()
sha256sums=()

dir="$HOME/pj/code/dotcfg"

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
