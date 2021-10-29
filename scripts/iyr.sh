#!/usr/bin/env bash

WATCHED_DIR="$HOME/Downloads"
test_run=true

watch_files() {
  local match_glob="$1"
  local dest="$2"
  for glob in $(ls $WATCHED_DIR/$match_glob); do
    if ($test_run); then
      echo "mv \"$glob\" \"$dest\""
    else
      mv "$glob" "$dest"
    fi
  done
}

watch_files "*.{png,jpg,jpeg,gif,mpeg}" "$HOME/Pictures"
watch_files "*.{mp4,mkv,webm,avi}" "$HOME/Videos"
watch_files "*.{pdf,docx,doc}" "$HOME/Documents"
