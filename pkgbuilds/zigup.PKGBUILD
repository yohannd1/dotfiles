# Based off the furnace-git one in the AUR

pkgname=zigup
pkgver=d0b471d
pkgrel=1
epoch=1
pkgdesc="Fork of zigup"
url="https://github.com/YohananDiamond/zigup-fork"
depends=()
makedepends=('git') # technically depends off of zig but bruh
optdepends=()
provides=(zig)
arch=('x86_64')
license=('GPL')
source=()
sha256sums=()

dir="$HOME/pj/code/zigup-fork"

pkgver() {
  cd "$dir"
  git rev-parse --short HEAD
}

prepare() {
  :
}

build() {
  cd "$dir"
  zig build
}

package() {
  cd "$pkgdir"
  mkdir -p "$pkgdir/usr/bin"
  cp "$dir/zig-out/bin/zigup" -t "$pkgdir/usr/bin"
}
