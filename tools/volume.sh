#!/usr/bin/env bash

SINK=`pactl list short sinks | grep RUNNING | cut -f 1`

if [[ $1 = "toggle" ]]; then
	pactl set-sink-mute "$SINK" toggle
else
	pactl set-sink-volume "$SINK" "$1"
fi

if which notify-send &>/dev/null; then

	eval `pacmd list-sinks | sed -ne '
	/\* index: \([0-9]\+\)/,/index: / {
	s#\s*muted: \(.\+\)#MUTED=\1#p;
	s#^\s*volume: .*\(\b[0-9]\+\)\%.*#VOLUME=\1#p }'`

	notify() {
		notify-send -a "Volume" -u low -h string:synchronous:volume "$@"
	}

	if [[ $MUTED = no ]]; then
		notify -h int:value:"$VOLUME" " "
	else
		notify "Muted"
	fi

fi
