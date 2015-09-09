#!/bin/bash

ENV_PATH="`pwd`"

INIT_FILE="$ENV_PATH/init.sh"
INIT_PATH="$ENV_PATH/init"

TOOLS_PATH="$ENV_PATH/tools"

# Create the init file
function create_init()
{
	echo "# Init env

# tools
export PATH=\"$PATH:$TOOLS_PATH\"

# aliases
source \"$INIT_PATH/base.sh\"
source \"$INIT_PATH/git_aliases.sh\"
source \"$INIT_PATH/save-go.sh\"
source \"$INIT_PATH/extra_aliases.sh\"
source \"$INIT_PATH/42_aliases.sh\"

# prompt
source \"$INIT_PATH/prompt.sh\"
" > $INIT_FILE
}

# Copy a file
function install_file()
{
	echo "Install $1 to $2"
	cp "$ENV_PATH/$1" "$2"
}

# Put in line in a file
# if the file does not already contains it
function install_line()
{
	if grep "$1" "$2" > /dev/null 2> /dev/null; then
		echo "$2 already up to date"
	else
		echo "Update $2"
		echo >> $2
		echo "$1" >> $2
	fi
}

# Main

install_file "$ENV_PATH/conf_gitconfig" "$HOME/.gitconfig"
install_file "$ENV_PATH/conf_gitignore" "$HOME/.gitconfig_excludes"

create_init
install_line "source \"$INIT_FILE\"" "$HOME/.bashrc"
install_line "source \"$INIT_FILE\"" "$HOME/.zshrc"
