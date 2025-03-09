pkgname=sxiv
pkgver=b1e742e
pkgrel=1
epoch=1
pkgdesc="Fork of sxiv"
url="https://github.com/yohannd1/sxiv-fork"
depends=()
makedepends=('git')
optdepends=()
provides=(zig)
arch=('x86_64')
license=('GPL')
source=()
sha256sums=()

dir=${SOURCE_DIR:-$HOME/pj/code/sxiv-fork}

pkgver() {
  cd "$dir"
  git rev-parse --short HEAD
}

prepare() {
  :
}

build() {
  cd "$dir"
  make
}

package() {
  cd "$dir"
  make PREFIX="/usr" DESTDIR="$pkgdir" install
}
