#!/usr/bin/env bash

# Toggle mute microphones. If at least one microphone is not muted, all the
# microphones are muted. Otherwise, the default microphone is unmuted.

# Call pamixer with the passed arguments on every sources
iter_sources()
{
  while read id _rest; do
    if [[ $id =~ ^[0-9]+$ ]]; then
      pamixer --source "$id" "$@"
    fi
  done < <(pamixer --list-sources)
}

at_least_one_mic_on()
{
  iter_sources --get-mute | grep "false" &>/dev/null
}

case "$1" in
  "toggle")
    if at_least_one_mic_on; then
      iter_sources --mute
      state="Off"
    else
      pamixer --default-source --unmute
      state="On"
    fi
    dunstify -r 1234 -a "Indicator" -u low "Microphone" "$state"
    ;;
  "get")
    # Get the current state of the microphones.
    # Print "on" if at least one microphone is not muted, "off" otherwise.
    if at_least_one_mic_on; then
      echo "On"
    else
      echo "Off"
    fi
    ;;
esac
