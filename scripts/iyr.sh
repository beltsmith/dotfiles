#!/usr/bin/env bash
#
# Script to organise downloaded files

HOME="/home/belt"
WATCHED_DIR="$HOME/Downloads"

watch_files() {
  local dest="$1"
  local extensions="-name *.$2"
  for extension in ${@:3}; do
    extensions="${extensions} -o -name *.$extension"
  done
  find ${WATCHED_DIR} -maxdepth 1 $extensions | while read file; do
    echo "mv \"$file\" \"$dest\""
    if [ -z ${DRY_RUN+x} ]; then
      mv "$file" "$dest"
    fi

  done
}

watch_files "$HOME/Pictures" "png" "jpg" "jpeg" "gif" "mpeg" "webp" "wav" "raw"
watch_files "$HOME/Videos" "mp4" "mkv" "webm" "avi"
watch_files "$HOME/Documents" "pdf" "docx" "doc"
watch_files "$HOME/wine-exes" "exe" "msi"
watch_files "$HOME/isos" "iso"
watch_files "$WATCHED_DIR/zips" "zip" "rar" "7z" "gz" "tar"
watch_files "$WATCHED_DIR/3dprints" "stl"
