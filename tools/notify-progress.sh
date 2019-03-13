#!/usr/bin/env bash

APP_NAME="$1"
VALUE="$2"
shift 2

DIV=$((VALUE/10))
REM=$((10-DIV))

repeat() { printf "%$1s" "" | tr " " "$2"; }

BAR="<span color=\"cyan\">$(repeat $DIV -)</span>$(repeat $REM -)"
dunstify -r 1234 -a "$APP_NAME" -u low "$BAR" "$@"
