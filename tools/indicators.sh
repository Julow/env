#!/usr/bin/env bash

# Date
DATE=`date +"<b>%H:%M</b> %A %d %B (week %V)"`
dunstify -r 101010 -a "Indicator" -u low "$DATE"

# Battery
acpi -b | while read line; do
	if [[ $line =~ ^Battery\ [0-9]*:\ (.*),\ ([0-9]+)%.*$ ]] &&
		[[ ${BASH_REMATCH[2]} -ne 0 ]]; then
		PROGRESS=`progress-bar.sh "${BASH_REMATCH[2]}"`
		MSG="<b>Battery</b> $PROGRESS ${BASH_REMATCH[1]}"
		dunstify -r 101011 -a "Indicator" -u low "$MSG"
	fi
done

# Wifi
WIFI=`netctl list | grep -e "^\S"`
if [[ -n $WIFI ]]; then
	dunstify -r 101012 -a "Indicator" -u low "<b>Wifi</b> $WIFI"
fi
