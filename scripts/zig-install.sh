#!/usr/bin/env sh

exists() { command -v "$1" >/dev/null 2>/dev/null; }

for dep in jq sha256sum curl fd; do
  if ! exists "$dep"; then
    printf >&2 "Missing dependency: %s\n" "$dep"
    exit 1
  fi
done

ZIG_IDX='https://ziglang.org/download/index.json'
ZIG_PACKDIR="${XDG_CACHE_HOME:-$HOME/.cache}/packs/zig"
ARCH=${ZIG_ARCH:-'x86_64-linux'}

# exit on errors
set -e

# make temp dir
tmp=$(mktemp -d)
cleanup() { rm -r "$tmp"; }
trap 'cleanup' EXIT
printf >&2 "Temp dir: %s\n" "$tmp"

# download the information
printf >&2 "Downloading metadata...\n"
json=$(curl "$ZIG_IDX" | jq -r '.master["'"$ARCH"'"]')

# download the file
printf >&2 "Downloading the binary tarball...\n"
curl "$(printf "%s" "$json" | jq -r '.tarball')" -o "$tmp/zig.tar.xz"

# fetch checksum and compare it
printf >&2 "Calculating checksums...\n"
onlineChecksum=$(printf "%s" "$json" | jq -r '.shasum')
calculatedChecksum=$(sha256sum "$tmp/zig.tar.xz" | awk '{ print $1 }')

# error out if the checksums are different
if [ "$onlineChecksum" != "$calculatedChecksum" ]; then
  printf >&2 "Invalid checksum!\n"
  printf >&2 "Online Checksum :: %s\n" "$onlineChecksum"
  printf >&2 "Calculated Checksum :: %s\n" "$calculatedChecksum"
  exit 1
else
  printf >&2 "Checksums successfully calculated!\n"
fi

if [ -d "$ZIG_PACKDIR" ]; then
  backupDirectory=$(mktemp -d "${TMP:-/tmp}/zig-backup-XXXXXX") || exit 1
  printf >&2 "A zig package is already installed; backing it up to %s\n" "$backupDirectory"
  printf >&2 "NOTE: you are responsible for manually deleting mentioned folder.\n"

  fd -H -d1 '.' "$ZIG_PACKDIR" | while read file; do
    mv -v "$file" -t "$backupDirectory"
  done

  printf >&2 "Backup finished!\n"
else
  mkdir "$ZIG_PACKDIR"
fi

(
  printf >&2 "Extracting contents of downloaded archive...\n"
  mkdir "$tmp/extract"
  cd "$tmp/extract"
  tar -xvJf "$tmp/zig.tar.xz"

  printf >&2 "Moving data to the right place...\n"

  if [ "$(fd -H -d1 | wc -l)" != 1 ]; then
    printf >&2 "The root of the file tree should have only one file, no?\n"
    exit 1
  else
    cd * # should go to the only directory available
  fi

  # documentation
  if [ -d 'docs' ]; then
    mv 'docs' -t "$ZIG_PACKDIR"
  else
    mkdir "$ZIG_PACKDIR/docs"
    mv 'langref.html' -t "$ZIG_PACKDIR/docs"
  fi
  mv 'LICENSE' -t "$ZIG_PACKDIR/docs"

  # binaries
  mkdir "$ZIG_PACKDIR/bin"
  mv 'zig' -t "$ZIG_PACKDIR/bin"

  # libraries
  mv 'lib' -t "$ZIG_PACKDIR"
  ln -s '../lib' "$ZIG_PACKDIR/bin/lib" # `zig` needs a `lib` directory alongside it

  printf >&2 "Installation finished!\n"

  if [ "$(fd -H -d1 | wc -l)" != 0 ]; then
    printf >&2 "There are still some files that didn't get picked up:\n"
    fd -H
  fi
)
