#!/bin/sh
# A fuzzy file-finder and opener based on dmenu
# Requires: dmenu, exo-open
find ~/notes/ -name '*.pdf' | sed 's/\/home\/alex\/notes\///g' | sort -f | dmenu -i -l 20 -nb '#3f3f3f' -nf '#dcdccc' -sf '#aecf94' -sb '#3f3f3f' -w 400 -z -fn 'Source Code Pro-9' | sed -e 's/^/\/home\/alex\/notes\//' | xargs exo-open 
# find ~/notes/ -name '*.pdf' | sed 's/\/home\/alex\/notes\///g' | sort -f | dmenu -i -l 20 -nb '#2e3436' -nf '#9999CC' | sed -e 's/^/\/home\/alex\/notes\//'

