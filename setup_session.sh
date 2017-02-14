#!/bin/bash

export ENV_PATH="`pwd -P`"

export SESSION_DIR="$ENV_PATH/session"

for setup in "$SESSION_DIR"/*.sh
do
	echo "# Setup $setup"
	bash "$setup"
done
