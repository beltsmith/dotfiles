#!/usr/bin/env bash
set -xeuo pipefail

SCREEN_FILE=/tmp/screen

if [ -s $SCREEN_FILE ]; then
    awk -i inplace 'BEGIN { FS = "-"; OFS = "-" }
                          { a = $2 + 1; b = a % 3
                            print $1, b }' $SCREEN_FILE
else
    echo "HEAD-0" > $SCREEN_FILE
fi

xsetwacom set $(xsetwacom list devices  | grep "STYLUS" | cut -f 2 -d : | cut -f 1) MapToOutput $(cat $SCREEN_FILE)
