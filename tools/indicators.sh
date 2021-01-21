#!/usr/bin/env bash

INFOS=()

# Date
DATE=`date +"<b>%H:%M</b> %A %d %B (week %V)"`
INFOS+=("$DATE")

# Battery
while read line; do
	if [[ $line =~ ^Battery\ [0-9]*:\ (.*),\ ([0-9]+)%.*$ ]] &&
		[[ ${BASH_REMATCH[2]} -ne 0 ]]; then
		PROGRESS=`progress-bar.sh "${BASH_REMATCH[2]}"`
    INFOS+=("<b>Battery</b> $PROGRESS ${BASH_REMATCH[1]}")
	fi
done < <(acpi -b)

# Network
if which nmcli &>/dev/null; then
  CONN=0
  while IFS=: read _ type state _ _ _ ssid _; do
    if [[ $state = connected ]]; then
      INFOS+=("<b>Connected to $type</b> <span foreground=\"green\">$ssid</span>")
      CONN=1
    fi
  done < <(nmcli -g all d)
  if [[ $CONN -eq 0 ]]; then
    INFOS+=("<b>Not connected</b>")
  fi
fi

dunstify -r "101010" -a "Indicator" -u low "" "`IFS=$'\n'; echo "${INFOS[*]}"`"
