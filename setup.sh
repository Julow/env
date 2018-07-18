#!/bin/bash

export ENV_PATH="`pwd -P`"

export SESSION_DIR="$ENV_PATH/session"

for setup in "$SESSION_DIR"/*.sh
do
	echo "$setup"

	bash "$setup"

	case $? in
		0)		echo "> Ok";;
		100)	echo "x Disabled";;
		101)	echo "> Up-to-date";;
		*)		echo "x Failed";;
	esac

done
