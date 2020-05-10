#!/usr/bin/env bash

# Increments or decrements the brightness
# `brightness.sh 10` increase brightness by 10%
# `brightness.sh -10` decrease brightness by 10%

set -e

INC=$1
if [[ $INC -gt 0 ]]; then
  INC=+$INC
fi

brightnessctl set -- "$INC%"

if which dunstify &>/dev/null; then
  IFS=, read _ _ _ VAL _ < <(brightnessctl info -m)
  VAL=${VAL%\%}
  dunstify -r 1234 -a "Brightness" -u low "" "`progress-bar.sh "$VAL"`"
fi
