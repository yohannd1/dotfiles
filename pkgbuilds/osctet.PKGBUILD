pkgname=osctet
pkgver=1.1.2
pkgrel=1
pkgdesc="..."
url="https://github.com/jangler/osctet"
arch=('x86_64')
license=('AGPLv3')
depends=()
optdepends=()
source=(
  "$pkgname-$pkgver.zip::https://github.com/jangler/osctet/releases/download/v${pkgver}/osctet-v${pkgver}-linux-x86_64.zip"
)
sha256sums=(
  'SKIP'
)
conflicts=()
OPTIONS=()

package() {
  for file in demosongs/*.osctet; do
    install -Dm644 "$file" "$pkgdir/usr/share/osctet/$file"
  done
  install -Dm755 osctet "$pkgdir/usr/bin/osctet"
}
