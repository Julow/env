#!/usr/bin/env bash

# UI wrapper for netctl

set -e

PROFILE_LIST=
# Empty if none
CURRENT_PROFILE=

load_profiles ()
{
	PROFILE_LIST=`netctl list`
	CURRENT_PROFILE=""
	while IFS="" read line; do
		if ! [[ $line = "  "* ]]; then
			CURRENT_PROFILE=${line:2}
			break
		fi
	done <<<"$PROFILE_LIST"
}

wait_failed ()
{
	echo "Failed, press any key to continue"
	read -n 1
}

# Remove menu

remove_menu ()
{
	REMOVE=`echo "$PROFILE_LIST" | select_in`
	if [[ -z $REMOVE ]]; then return 0; fi
	REMOVE=${REMOVE:2}
	echo "Removing $REMOVE"
	su -c "netctl stop '$REMOVE'; netctl disable '$REMOVE'; rm '/etc/netctl/$REMOVE'" || wait_failed
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

while true; do

	clear

	load_profiles

	SWITCH_TO=`list | select_in`
	if [[ -z $SWITCH_TO ]]; then break; fi
	SWITCH_TO=${SWITCH_TO:2}

	case "$SWITCH_TO" in
		"[Disconnect all]")
			echo "Disconnecting"
			su -c "netctl stop-all" || wait_failed
			;;
		"[Disconnect $CURRENT_PROFILE]")
			echo "Disconnecting from $CURRENT_PROFILE"
			su -c "netctl stop '$CURRENT_PROFILE'" || wait_failed
			;;
		"[New]")
			su -c "wifi-menu" || wait_failed
			;;
		"[Remove]")
			remove_menu
			;;
		*)
			echo "Connecting to $SWITCH_TO"
			su -c "netctl switch-to '$SWITCH_TO'" || wait_failed
			echo "Wait online"
			if netctl wait-online "$SWITCH_TO"
			then exit 0
			else wait_failed
			fi
			;;
	esac

done
