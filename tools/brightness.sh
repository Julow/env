#!/usr/bin/env bash

# Increments or decrements the brightness
# `brightness.sh 10` increase brightness by 10%
# `brightness.sh -10` decrease brightness by 10%

set -e

INC="$1"

set /sys/class/backlight/*/brightness

if [[ $# -eq 0 ]]; then
	echo "Cannot change brightness" >&2
	exit 1
fi

CUR=`cat "$1"`
MAX=`cat "${1%brightness}max_brightness"`
NEW=$((MAX * INC / 100 + CUR))

if [[ $NEW -gt $MAX ]]; then NEW=$MAX; fi
if [[ $NEW -lt 0 ]]; then NEW=0; fi

echo $NEW > "$1"
echo "$NEW / $MAX"

if which dunstify &>/dev/null; then
	VAL=$((NEW * 100 / MAX))
	dunstify -r 1234 -a "Brightness" -u low "" "`progress-bar.sh "$VAL"`"
fi
