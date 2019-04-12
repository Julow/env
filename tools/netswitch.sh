#!/usr/bin/env bash

set -e

SWITCH_TO=`netctl list | select_in`

SWITCH_TO=${SWITCH_TO:2}
[[ -n $SWITCH_TO ]]

echo "Connecting to $SWITCH_TO"
su -c "netctl switch-to '$SWITCH_TO'"

echo "Wait online"
netctl wait-online "$SWITCH_TO"
