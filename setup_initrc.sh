#!/bin/bash

export ENV_PATH="`pwd -P`"

export INIT_FILE="$ENV_PATH/_initrc.sh"
export INIT_DIR="$ENV_PATH/initrc"

DEFAULT_ARGS="alias:base,git,save-go rc:.bashrc?,.zshrc? brew:? brew:.linuxbrew? tools prompt opam:?"

#

ARGS="$@"
if [[ $# -eq 0 ]]; then
	ARGS="$DEFAULT_ARGS"
	echo "Use default arguments: $ARGS"
fi

echo "# env init" > $INIT_FILE
echo "# args: $ARGS" >> $INIT_FILE

function run_init
{
	if [[ "$1" = "default" ]]; then
		run $DEFAULT_ARGS
	else
		echo "# $1"
		F="$INIT_DIR/$1.sh"
		if [[ -f "$F" ]]; then
			echo >> $INIT_FILE
			echo "# $1" >> $INIT_FILE
			bash "$F" ${2//,/ } >> $INIT_FILE
		else
			echo "Warning: Invalid param: $1"
		fi
	fi
}

function run
{
	for arg in "$@"; do
		run_init ${arg//:/ };
	done
}

run $ARGS
