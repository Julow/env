#!/usr/bin/env bash

# Run $EDITOR in xterm, return immediately

if [[ -z $EDITOR ]]; then
  echo "\$EDITOR is not set" >&2
  exit 2
fi

xterm -e "$EDITOR" "$@" &
