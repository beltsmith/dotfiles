#!/bin/bash
 
# grabs the nasa image of the day by RSS feed and updates the gnome
# background. add this to your cron jobs to have this happen daily.  this is,
# obviously, a hack, that is likely to break at the slightest change of NASA's
# RSS implementation. yay standards!

#EDITED FOR feh
 
rss=`wget -q -O - http://www.nasa.gov/rss/dyn/lg_image_of_the_day.rss`

# this command is benign if the directory already exists.
mkdir -p $HOME/.backgrounds

first=1
 
while read -r img_url ; do
	img_name=`echo $img_url | grep -o [^/]*\.\w*$`
	echo "Pulling $img_name from $img_url"
	wget -q -O $HOME/.backgrounds/$img_name $img_url
	if ((first == 1)) ; then
		feh --bg-scale $HOME/.backgrounds/$img_name
		echo "Setting background to latest image $img_name"
		first=0
	fi
done < <(echo $rss | grep -o '<enclosure [^>]*>' | grep -o 'http://[^\"]*')
