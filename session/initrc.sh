#!/bin/bash

export INIT_FILE="$ENV_PATH/_initrc.sh"
export INIT_DIR="$SESSION_DIR/initrc"

DEFAULT_ARGS="i_guard env bash_completion lesspipe
	alias:git,save-go tools subl:?
	brew:? brew:.linuxbrew? opam:?
	prompt
	rc:.bashrc?,.zshrc?"

#

function run_init
{
	# echo "# $1"
	F="$INIT_DIR/$1.sh"
	if [[ -f "$F" ]]; then
		echo >> $INIT_FILE
		echo "# $1" >> $INIT_FILE
		bash "$F" ${2//,/ } >> $INIT_FILE
	else
		echo "Warning: Invalid param: $1"
	fi
}

function run
{
	echo "# args: $ARGS" > $INIT_FILE
	for arg in "$@"; do
		run_init ${arg//:/ }
	done
}

if [[ $# = 0 || $1 = "default" ]]; then
	echo "Use defaults"
	run $DEFAULT_ARGS
else
	run $ARGS
fi
