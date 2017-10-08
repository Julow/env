#!/bin/bash

export ENV_PATH="`pwd -P`"

export SESSION_DIR="$ENV_PATH/session"

for setup in "$SESSION_DIR"/*.sh
do
	printf "# %-50s " "$setup"

	bash "$setup" &>/tmp/logs

	case $? in
		0)		echo "Ok";;
		100)	echo "Disabled";;
		101)	echo "Up-to-date";;
		*)		echo "Failed";;
	esac

	cat /tmp/logs

done
