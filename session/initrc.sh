#!/bin/bash

set -e

export INIT_FILE="$ENV_PATH/_initrc.sh"
export INIT_DIR="$SESSION_DIR/initrc"

# Generate initrc
# *.gen.sh files are executed, other .sh files are simply concatenated

for rc in $(ls $INIT_DIR)
do
	echo "# $rc"
	if [[ $rc = *.gen.sh ]]
	then (cd "$INIT_DIR"; bash -e "$INIT_DIR/$rc")
	else cat "$INIT_DIR/$rc"
	fi
	echo
done > $INIT_FILE

# Source it from .bashrc and .zshrc

LINE="source \"$INIT_FILE\""

function install_line()
{
	if ! grep "$1" "$2" > /dev/null 2> /dev/null; then
		echo "Update $2"
		echo "$1" >> "$2"
	fi
}

install_line "$LINE" ~/.bashrc
if [[ -f ~/.zshrc ]]; then install_line "$LINE" ~/.zshrc; fi
