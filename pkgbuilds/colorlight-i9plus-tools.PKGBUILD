_pkgname=colorlight-i9plus-tools
pkgname=$_pkgname-git
pkgver=0.1
pkgrel=1
pkgdesc='Colorlight i9+ toolset'
arch=(x86_64)
license=()
groups=()
provides=()
conflicts=()
depends=()
makedepends=(git)
optdepends=()
options=(!strip)
source=(
  "git+https://github.com/wuxx/Colorlight-FPGA-Projects"
  "git+https://github.com/openocd-org/openocd#commit=3a4f445bd92101d3daee3715178d3fbff3b7b029"
)
sha256sums=('SKIP' 'SKIP')

# TODO: this package is a heavy WIP at the moment. Still figuring out what to bundle and ironing bugs out.

prepare() {
  (
    cd Colorlight-FPGA-Projects
    sed -i '
      s|^OPENOCD_ROOT=.*$|OPENOCD_ROOT=/opt/colorlight-i9plus-tools/openocd|
      s|^CURRENT_DIR=.*$|CURRENT_DIR=/opt/colorlight-i9plus-tools/tools|
    ' tools/ch347prog
    chmod -x tools/env.sh
  )

  (
    cd openocd
    git apply --reject --whitespace=fix ../Colorlight-FPGA-Projects/tools/openocd-patch-for-ch347/ch347.patch
  )
}

build() {
  (
    cd openocd
    ./bootstrap
    ./configure --enable-ch347 --disable-werror
    make
  )
}

package() {
  install -Dm755 -T Colorlight-FPGA-Projects/tools/ch347prog "$pkgdir/usr/bin/ch347prog"
  ln -s ch347prog "$pkgdir/usr/bin/ch347prog-flash"
  ln -s ch347prog "$pkgdir/usr/bin/ch347prog-probe"
  ln -s ch347prog "$pkgdir/usr/bin/ch347prog-sram"

  mkdir -p "$pkgdir/opt/colorlight-i9plus-tools"

  # bundle only the part of openocd that ch347 uses
  mkdir -p "$pkgdir/opt/colorlight-i9plus-tools/openocd"
  cp -r openocd/tcl -t "$pkgdir/opt/colorlight-i9plus-tools/openocd"
  mkdir -p "$pkgdir/opt/colorlight-i9plus-tools/openocd/src"
  cp openocd/src/openocd -t "$pkgdir/opt/colorlight-i9plus-tools/openocd/src"

  cp -r \
    Colorlight-FPGA-Projects/{README.md,colorlight_i9plus_v6.1.md,colorlight_i9_v7.2.md,LICENSE} \
    Colorlight-FPGA-Projects/{doc,schematic,tools} \
    -t "$pkgdir/opt/colorlight-i9plus-tools"
}
