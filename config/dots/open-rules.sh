#!/usr/bin/env sh

# this file is sourced by the 'open' script.
# place all your rules here.
# see said script for help.

block mime 'application/x-sharedlib'
block mime 'application/x-shared-library-la'
block mime 'application/x-executable'
block mime 'application/x-shellscript'

if match url 'http|https'; then
  try gui "$BROWSER" "$FILE"
  try tty "$TERMBROWSER" "$FILE"
fi

if match ext 'pdf' || match mime 'application/pdf'; then
  try gui zathura "$FILE"
  try tty runpage pdftotext -l 10 -nopgbrk -q -- "$FILE" -
  try tty mutool draw -F txt -i -- "$FILE" 1-10
  try tty exiftool "$FILE"
fi

if match mime 'audio/*' ||
  match ext 'aac|flac|m4a|mid|midi|mpa|mp2|mp3|ogg|wav|wma'; then
  try tty mocplay "$FILE" "opener"
  try pop mpv --no-video "$FILE"
  try tty mediainfo "$FILE"
  try tty exiftool "$FILE"
fi

if match mime 'video/*' ||
  match ext 'avi|mkv|mp4'; then
  try gui smplayer "$FILE"
  try gui mpv "$FILE"
  try tty mpv --no-video "$FILE"
  try tty runpage mediainfo "$FILE"
fi

if match ext 'a|ace|alz|arc|arj|bz|bz2|cab|cpio|deb|gz|jar|lha|lz|lzh|lzma'\
             '|lzo|rpm|rz|t7z|tar|tbz|tbz2|tgz|tlz|txz|tZ|tzo|war|xpi|xz|Z|zip'; then
  try runpage atool --list -- "$FILE"
  try runpage bsdtar --list --file "$FILE"
fi

match ext 'rar' && try pop runpage unrar l -p- -- "$FILE"
match ext '7z' && try pop runpage 7z l -p -- "$FILE"

if match ext 'torrent'; then
  try pop rtorrent "$FILE"
  try pop transmission-show -- "$FILE"
fi

match ext 'odt|ods|odp|sxw' && try pop runpage odt2txt "$FILE"
match ext 'txt|log' && try pop runpage cat "$FILE"
match ext 'kra' && try gui krita "$FILE"

if match ext 'md'; then
  try tty runpage glowrapper "$FILE"
  try gui md-preview "$FILE"
fi

if match ext 'htm|html|xhtml'; then
  try tty runpage w3m -dump "$FILE"
  try tty runpage lynx -dump -- "$FILE"
  try tty runpage elinks -dump "$FILE"
  try gui "$BROWSER" "$FILE"
fi

if match ext 'json'; then
  try pop runpage jq --color-output . "$FILE"
  try pop runpage python -m json.tool -- "$FILE"
fi

if match mime 'image/vnd.djvu'; then
  try tty runpage djvutxt "$FILE"
  try tty runpage exiftool "$FILE"
fi

if match mime 'image/*'; then
  try gui sxiv "$FILE"
  try tty runpage viu -n "$FILE"
  try tty runpage img2txt --gamma=0.6 -- "$FILE"
  try tty runpage exiftool "$FILE"
fi

match mime 'inode/directory' && try pop "$FILEMAN" "$FILE"
match mime 'inode/x-empty' && try pop runpage cat "$FILE"
match mime 'text/troff' && try pop runpage man -l "$FILE"
match mime 'text/*' && try pop runpage cat "$FILE"
match mime '*/xml' && try pop runpage cat "$FILE"
