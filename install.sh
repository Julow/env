#!/bin/bash

ENV_PATH="`pwd`"
INIT_FILE="$ENV_PATH/init.sh"
TOOLS_PATH="$ENV_PATH/tools"

function install()
{
	echo "Install $1 to $2"
	cp "$ENV_PATH/$1" "$2"
}

function install_init()
{
	if grep "juloo env" "$1" > /dev/null; then
		echo "Init already installed in $1"
	else
		echo "Install init in $1"
		echo "
# juloo env
export PATH=\"$PATH:$TOOLS_PATH\"
if [[ -f \"$INIT_FILE\" ]]; then
	source \"$INIT_FILE\"
fi" >> $1
	fi
}

install "conf_gitconfig" "$HOME/.gitconfig"
install "conf_gitignore" "$HOME/.gitconfig_excludes"

install_init "$HOME/.bashrc"
install_init "$HOME/.zshrc"
