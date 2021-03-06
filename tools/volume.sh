#!/usr/bin/env bash

SINK=`pactl list short sinks | grep RUNNING | cut -f 1`

if [[ $1 = "toggle" ]]; then
	pactl set-sink-mute "$SINK" toggle
else
	pactl set-sink-volume "$SINK" "$1"
fi

if which dunstify &>/dev/null; then

	eval `pacmd list-sinks | sed -ne '
	/\* index: \([0-9]\+\)/,/index: / {
	s#\s*muted: \(.\+\)#MUTED=\1#p;
	s#^\s*volume: .*\(\b[0-9]\+\)\%.*#VOLUME=\1#p }'`

	if [[ $MUTED = no ]]; then
		MSG=`progress-bar.sh "$VOLUME"`
	else
		MSG=Muted
	fi

	dunstify -r 1234 -a "Volume" -u low "" "$MSG"

fi
