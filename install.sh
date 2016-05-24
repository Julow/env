#!/bin/bash

export ENV_PATH="`pwd -P`"

export INIT_FILE="$ENV_PATH/_init.sh"
export INIT_DIR="$ENV_PATH/init"

DEFAULT_ARGS="alias:base,git,save-go rc:.bashrc?,.zshrc? brew:? tools prompt"

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
	F="$INIT_DIR/$1.sh"
	if [[ -f "$F" ]]; then
		echo >> $INIT_FILE
		echo "# $1" >> $INIT_FILE
		bash "$F" ${2//,/ } >> $INIT_FILE
	else
		echo "Warning: Invalid param: $1"
	fi
}

for arg in $ARGS; do run_init ${arg//:/ }; done
