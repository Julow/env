#!/usr/bin/env bash

status ()
{
	date
	acpi
}

CLEAR=`tput clear`

render ()
{
	STATUS=`status`
	FULL_LINE=`printf -- "-%.0s" {1..30}`
	printf "%s/$FULL_LINE\\\\\n" "$CLEAR"
	IFS=$'\n'
	printf "| %-28s |\n" $STATUS
	unset IFS
	printf "\\$FULL_LINE/"
}

tput civis

render

# Render every seconds, until user input
until read -s -N1 -t1; do
	render
done

tput cnorm
