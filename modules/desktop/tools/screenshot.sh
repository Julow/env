#!/usr/bin/env bash

# screenshot.sh [mode]
#
# Takes a screenshot,
#  put it in /tmp/screenshot,
#  and copy it to the clipboard
#
# Modes:
#  screen		Full screen
#  interactive	Rectangle selection
#  				(default)

SCREENSHOT_DIR="/tmp/screenshot"

function get_screenshot_name
{
	BASE_NAME=`date +"%Y-%m-%d-%H-%M"`
	for i in "" -{01..99}; do
		F="$SCREENSHOT_DIR/$BASE_NAME$i.png"
		if ! [[ -f "$F" ]]; then
			echo "$F"
			return
		fi
	done
}

function take_screenshot
{
	mkdir -p "$SCREENSHOT_DIR"
	SCREENSHOT_FILE=`get_screenshot_name`
	import -silent "$@" "$SCREENSHOT_FILE"
	xclip -selection clipboard -t image/png "$SCREENSHOT_FILE"
}

case "$1" in
	screen)			take_screenshot -window root;;
	interactive|"")	take_screenshot;;
	*)				echo "Invalid argument '$1'";;
esac
