#!/bin/bash

export DISPLAY=:0.0
# customize these
WGET=/usr/bin/wget
ICS2ORG=$HOME/dotfiles/scripts/scripts/ical2org.awk
# ICSFILE=$HOME/org/calendar.ics
# ORGFILE=$HOME/org/calendar.org
# URL=https://calendar.google.com/calendar/ical/alex%40wellsaidlabs.com/private-b94a18d6faa252a01a313e204dba4951/basic.ics

ICSFILE="$1"
ORGFILE="$2"
URL="$3"

# no customization needed below

$WGET -O $ICSFILE $URL
$ICS2ORG <$ICSFILE >$ORGFILE

notify-send 'You got calendar!' -i ~/Pictures/aol.png
