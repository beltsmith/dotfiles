#!/bin/bash

DISPLAY=:0
# customize these
WGET=/usr/bin/wget
ICS2ORG=$HOME/dotfiles/scripts/scripts/ical2org.awk
ICSFILE=$HOME/org/calendar.ics
ORGFILE=$HOME/org/calendar.org
URL=https://calendar.google.com/calendar/ical/alex.girdler%40sonder.com/private-4ac89d61623da045a12cf2169324befe/basic.ics

# no customization needed below

$WGET -O $ICSFILE $URL
$ICS2ORG < $ICSFILE > $ORGFILE

notify-send 'You got calendar!' -i ~/Pictures/aol.jpg
