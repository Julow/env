#!/usr/bin/env bash

render ()
{
	printf "/%s\\\\\n| %28s |\n\\%s/" \
		"`printf -- "-%.0s" {1..30}`" \
		"`date`" \
		"`printf -- "-%.0s" {1..30}`"
}

tput civis

render

# Render every seconds, until user input
until read -N1 -t1; do
	printf "%s" "`tput clear; render`"
done
