#!/usr/bin/env bash

N=0

indicator ()
{
  dunstify -r "10101$N" -a "Indicator" -u low "" "$1"
  N=$((N+1))
}

# Date
DATE=`date +"<b>%H:%M</b> %A %d %B (week %V)"`
indicator "$DATE"

# Battery
while read line; do
	if [[ $line =~ ^Battery\ [0-9]*:\ (.*),\ ([0-9]+)%.*$ ]] &&
		[[ ${BASH_REMATCH[2]} -ne 0 ]]; then
		PROGRESS=`progress-bar.sh "${BASH_REMATCH[2]}"`
    indicator "<b>Battery</b> $PROGRESS ${BASH_REMATCH[1]}"
	fi
done < <(acpi -b)

# Network
if which nmcli &>/dev/null; then
  CONN=0
  while IFS=: read _ type state _ _ _ ssid _; do
    if [[ $state = connected ]]; then
      indicator "<b>Connected to $type</b> $ssid"
      CONN=1
    fi
  done < <(nmcli -g all d)
  if [[ $CONN -eq 0 ]]; then
    indicator "<b>Not connected</b>"
  fi
fi
