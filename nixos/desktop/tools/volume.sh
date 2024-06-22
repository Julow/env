#!/usr/bin/env bash

case "$1" in
  "toggle") pamixer --toggle-mute ;;
  "dec") pamixer --decrease 5 --gamma 2.2 ;;
  "inc") pamixer --increase 5 --gamma 2.2 ;;
esac

vol=`pamixer --get-volume-human`
if [[ $vol = muted ]]; then
  args=(Muted)
else
  args=(-h "int:value:${vol%"%"}")
fi

dunstify -r 1234 -a "Indicator" -u low "Volume" "${args[@]}"
