#!/bin/bash
 
wid=$(xdotool getactivewindow)
wClass=$(xprop -id $wid WM_CLASS | cut -d\" -f2)
action=$1
 
if [ "$action" != "paste" ] && [ "$action" != "copy" ]
then
  echo "Invalid command"
  exit 1
fi
 
if [ "$wClass" == "termite" ]
then
  if [ "$action" == "copy" ]
  then
    /usr/bin/xdotool key --clearmodifiers shift+ctrl+c
  else
    /usr/bin/xdotool key --clearmodifiers shift+ctrl+v
  fi
else  
  if [ "$action" == "copy" ]
  then
    /usr/bin/xdotool key --clearmodifiers ctrl+c
  else
    /usr/bin/xdotool key --clearmodifiers ctrl+v
  fi
fi
