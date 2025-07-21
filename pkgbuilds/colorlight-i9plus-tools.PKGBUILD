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
options=()
source=(
  "git+https://github.com/wuxx/Colorlight-FPGA-Projects"
  "git+https://github.com/openocd-org/openocd#commit=3a4f445bd92101d3daee3715178d3fbff3b7b029"
)
sha256sums=('SKIP' 'SKIP')

prepare() {
  (
    cd Colorlight-FPGA-Projects
    sed -i 's ^OPENOCD_ROOT=.*$ OPENOCD_ROOT=/opt/colorlight-i9plus-tools/openocd ' tools/ch347prog
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
  install -Dm755 -T Colorlight-FPGA-Projects/tools/ch347prog-flash "$pkgdir/usr/bin/ch347prog-flash"
  install -Dm755 -T Colorlight-FPGA-Projects/tools/ch347prog-probe "$pkgdir/usr/bin/ch347prog-probe"
  install -Dm755 -T Colorlight-FPGA-Projects/tools/ch347prog-sram "$pkgdir/usr/bin/ch347prog-sram"
  mkdir -p "$pkgdir/opt/colorlight-i9plus-tools"
  cp -r openocd "$pkgdir/opt/colorlight-i9plus-tools/openocd"
  cp -r Colorlight-FPGA-Projects/doc "$pkgdir/opt/colorlight-i9plus-tools/doc"
  cp -r Colorlight-FPGA-Projects/schematic "$pkgdir/opt/colorlight-i9plus-tools/schematic"
  cp Colorlight-FPGA-Projects/{README.md,colorlight_i9plus_v6.1.md,colorlight_i9_v7.2.md,LICENSE} -t "$pkgdir/opt/colorlight-i9plus-tools"
}
