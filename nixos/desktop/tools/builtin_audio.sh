#!/usr/bin/env bash

# Toggle mute on the built-in speaker and microphone using pamixer.

# Work on the output of [--list-sources] and [--list-sinks].
parse_builtin_id ()
{
  local id
  read id _ < <(grep '"Built-in Audio Analog Stereo"')
  echo "$id"
}

toggle_mute_builtin ()
{
  local list_opt=$1 select_opt=$2 display_name=$3
  local id state
  id=$(pamixer "$list_opt" | parse_builtin_id)
  pamixer "$select_opt" "$id" --toggle-mute
  if [[ $(pamixer "$select_opt" "$id" --get-mute) = "true" ]]; then
    state="Muted"
  else
    state="Unmuted"
  fi
  dunstify -r 1234 -a "Indicator" -u low "$display_name" "$state"
}

case "$1" in
  "toggle_mic")
    toggle_mute_builtin --list-sources --source "Built-in Microphone" ;;
  "toggle_speaker")
    toggle_mute_builtin --list-sinks --sink "Built-in Speakers" ;;
esac
