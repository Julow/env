#!/usr/bin/env bash

INFOS=()

# Date
DATE=`date +"<b>%H:%M</b> %A %d %B (week %V)"`
INFOS+=("$DATE")

# Battery
while read line; do
	if [[ $line =~ ^Battery\ [0-9]*:\ (.*),\ ([0-9]+)%.*$ ]]; then
    STATUS=${BASH_REMATCH[1]}
    VAL=${BASH_REMATCH[2]}
    BAR_COLOR=
    if [[ $VAL -le 20 ]] && [[ $STATUS = Discharging ]]; then BAR_COLOR=red; fi
		PROGRESS=`BAR_COLOR=$BAR_COLOR progress-bar.sh "$VAL"`
    INFOS+=("<b>Battery</b> $PROGRESS $STATUS")
	fi
done < <(acpi -b)

# Network
CONN=0
while IFS=: read _ type state _ _ _ ssid _; do
  if [[ $state = connected ]]; then
    INFOS+=("<b>Connected to $type</b> <span foreground=\"green\">$ssid</span>")
    CONN=1
  elif [[ $state = connecting* ]]; then
    INFOS+=("<b>Connecting to $type</b> <span foreground=\"darkorange\">$ssid</span>...")
    CONN=1
  fi
done < <(nmcli -g all d)
if [[ $CONN -eq 0 ]]; then
  INFOS+=("<b>Not connected</b>")
fi

# Media
IFS="|" read player status title < \
  <(playerctl metadata -f "{{playerName}}|{{status}}|{{artist}} - {{title}}")
if [[ $status = "Playing" ]]; then
  INFOS+=("<b>${player^}</b> $title")

  # Volume muted status
  # TODO: Print sink name when [pamixer --get-default-sink] is available
  vol=`pamixer --get-volume-human`
  if [[ $vol = muted ]]; then
    INFOS+=("<b>Volume</b> Muted")
  fi
fi

# Rfkill status
for dev in /sys/class/rfkill/*; do
  read hard < "$dev/hard"
  read soft < "$dev/soft"
  if [[ $hard -ne 0 ]] || [[ $soft -ne 0 ]]; then
    read name < "$dev/name"
    read typ < "$dev/type"
    INFOS+=("<b>${typ^} blocked</b> $name")
  fi
done

# CPU temperature
temps=()
temp_st=ok
while read _ _ st temp _; do
  st=${st%,}
  if ! [[ $st = ok ]]; then temp_st=$st; fi
  temps+=("$temp")
done < <(acpi -t)
if [[ $temp_st = ok ]]; then temp_st=""; else temp_st=" <span foreground=\"red\">($temp_st)</span>"; fi
INFOS+=("<b>Temp</b> ${temps[*]}$temp_st")

# Microphone mute
read _ mic_mute < <(pactl get-source-mute $(pactl get-default-source))
if [[ $mic_mute = yes ]]; then mic_mute_s=Off; else mic_mute_s=On; fi
INFOS+=("<b>Microphone</b> $mic_mute_s")

dunstify -r "101010" -a "Status" -u low "" "`IFS=$'\n'; echo "${INFOS[*]}"`"
