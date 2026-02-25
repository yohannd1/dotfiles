# Based off furnace-git and lmms-git PKGBUILDs

pkgname=lmms-fork
pkgver=1.3.0.alpha.1.r1008.gdda3120f0
pkgrel=1
pkgdesc="The Linux MultiMedia Studio."
arch=('x86_64')
license=('GPL2')
url="https://lmms.io"
depends=(
  sdl2 libsndfile fmt hicolor-icon-theme alsa-lib fftw
  rtmidi stk
)
optdepends=(
  'pulseaudio: pulseaudio support'
  'jack2: jack support'
  'portaudio: portaudio support'
  'wine: VST plugin support'
  'carla: carla support'
)
makedepends=(
  git cmake extra-cmake-modules freetype2 git ladspa
  libsndfile libvorbis lame libsamplerate libogg libgig
  fluidsynth fltk
  qt5-base qt5-tools qt5-x11extras
  qt5-tools wine perl-list-moreutils perl-xml-parser perl-exporter-tiny
)
provides=('lmms')
conflicts=('lmms')

source=()

forkDir=${SOURCE_DIR:-$HOME/pj/code/lmms-fork}

_log() {
  printf >&2 "[at %s] " "$PWD"
  printf >&2 -- "$@"
  printf >&2 "\n"
}

pkgver() {
  cd "$forkDir"
  # git rev-parse --short HEAD
  git describe --long --tags | sed -r 's/^v//;s/([^-]*-g)/r\1/;s/-/./g'
}

prepare() {
  :

  # XXX: SKIPPING THIS but it might be needed when setting up the repo on a new machine
  # git submodule init
  # git config submodule.src/3rdparty/qt5-x11embed.url "${srcdir}/qt5-x11embed"
  # git config submodule.src/3rdparty/rpmalloc.url "${srcdir}/rpmalloc"
  # git -c protocol.file.allow=always submodule update
  # # setting lib dir
  # sed -e 's|lib64|lib|g' -i cmake/modules/DetectMachine.cmake
  # sed -e 's/\(${BASHCOMP_USER\)/\1/g' -i cmake/modules/BashCompletion.cmake
}

build() {
  _log "BUILDING"

  mkdir -p build && cd build

  cmake \
    -G Ninja \
    -DCMAKE_INSTALL_PREFIX=/usr \
    -DWANT_QT5=ON \
    -DWANT_SOUNDIO=OFF \
    -DCMAKE_INSTALL_LIBDIR=lib \
    -DCMAKE_MODULE_PATH=/usr/share/ECM/find-modules \
    -DCMAKE_PREFIX_PATH=/usr/include/wine/windows \
    -DWINE_INCLUDE_DIR=/usr/include/wine/windows \
    -DWINE_LIBRARY=/usr/lib32/wine \
    -DCMAKE_POLICY_VERSION_MINIMUM=3.5 \
    "$forkDir"

  cmake --build .
}

package() {
  _log "PACKAGING"

  DESTDIR="$pkgdir" cmake --install build
}
