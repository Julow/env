#!/usr/bin/env bash

# Increments or decrements the brightness
# `brightness.sh 10` increase brightness by 10%
# `brightness.sh -10` decrease brightness by 10%

set -e

INC=$1
if [[ $INC -gt 0 ]]; then
  INC=+$INC
fi

brightnessctl set -e -- "$INC%"

IFS=, read _ _ _ VAL _ < <(brightnessctl info -m)
dunstify -r 1234 -a "Indicator" -u low -h "int:value:${VAL%\%}" "Brightness"
