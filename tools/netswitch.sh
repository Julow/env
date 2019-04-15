#!/usr/bin/env bash

# UI wrapper for netctl

set -e

PROFILE_LIST=`netctl list`

# Empty if none
CURRENT_PROFILE=""
while IFS="" read line; do
	if ! [[ $line = "  "* ]]; then
		CURRENT_PROFILE=${line:2}
		break
	fi
done <<<"$PROFILE_LIST"

# Remove menu

remove_menu ()
{
	REMOVE=`echo "$PROFILE_LIST" | select_in`
	REMOVE=${REMOVE:2}
	[[ -n $REMOVE ]]
	echo "Removing $REMOVE"
	su -c "netctl stop '$REMOVE'; netctl disable '$REMOVE'; rm '/etc/netctl/$REMOVE'"
}

# Main menu

list ()
{
	if [[ -n $CURRENT_PROFILE ]]; then
		echo "> [Disconnect $CURRENT_PROFILE]"
		echo "> [Disconnect all]"
	fi
	echo "$PROFILE_LIST"
	echo "> [New]"
	echo "> [Remove]"
}

SWITCH_TO=`list | select_in`
SWITCH_TO=${SWITCH_TO:2}

case "$SWITCH_TO" in
	"")
		exit 1
		;;
	"[Disconnect all]")
		echo "Disconnecting"
		su -c "netctl stop-all"
		;;
	"[Disconnect $CURRENT_PROFILE]")
		echo "Disconnecting from $CURRENT_PROFILE"
		su -c "netctl stop '$CURRENT_PROFILE'"
		;;
	"[New]")
		su -c "wifi-menu"
		;;
	"[Remove]")
		remove_menu
		;;
	*)
		echo "Connecting to $SWITCH_TO"
		su -c "netctl switch-to '$SWITCH_TO'"
		echo "Wait online"
		netctl wait-online "$SWITCH_TO"
		;;
esac
