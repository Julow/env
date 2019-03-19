#!/usr/bin/env bash

DATE=`date +"<b>%H:%M</b> %A %d %B (week %V)"`
dunstify -r 101010 -a "Indicator" -u low "$DATE"

eval `acpi -b | sed -e 's/^Battery [0-9]*: \(.*\), \([0-9]\+\)%.*$/STATE="\1" LEVEL="\2"/'`
dunstify -r 101011 -a "Indicator" -u low "<b>Battery</b> `progress-bar.sh "$LEVEL"` $STATE"

WIFI=`netctl list | grep -e "^\S"`
if [[ -n $WIFI ]]; then
	dunstify -r 101012 -a "Indicator" -u low "<b>Wifi</b> $WIFI"
fi
