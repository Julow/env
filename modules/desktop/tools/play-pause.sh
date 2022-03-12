#!/usr/bin/env bash

set -e

IFS="|" read player status < <(playerctl status -f "{{playerName}}|{{status}}")

if [[ $status = "Playing" ]]; then
  playerctl pause
  MSG="Paused"
else
  playerctl play
  MSG="Playing"
fi

dunstify -r 303030 -a "${player^}" -u low "$MSG" ""
