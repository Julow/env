#!/bin/bash

ENV_PATH="`pwd`"

INIT_FILE="$ENV_PATH/init.sh"
INIT_PATH="$ENV_PATH/init"

TOOLS_PATH="$ENV_PATH/tools"

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

install_file "conf_gitconfig" "$HOME/.gitconfig"
install_file "conf_gitignore" "$HOME/.gitconfig_excludes"

echo "# Init env

# tools
export PATH=\"\$PATH:$TOOLS_PATH\"

# aliases
source \"$INIT_PATH/base.sh\"
source \"$INIT_PATH/git_aliases.sh\"
source \"$INIT_PATH/save-go.sh\"
source \"$INIT_PATH/extra_aliases.sh\"
source \"$INIT_PATH/42_aliases.sh\"

# prompt
source \"$INIT_PATH/prompt.sh\"
" > $INIT_FILE

if [[ -d "$HOME/.brew" ]]; then
	echo "# brew
export PATH=\"$HOME/.brew/bin:\$PATH\"
export LIBRARY_PATH=\"\$LIBRARY_PATH:$HOME/.brew/lib\"
export LD_LIBRARY_PATH=\"\$LD_LIBRARY_PATH:$HOME/.brew/lib\"
export C_INCLUDE_PATH=\"\$C_INCLUDE_PATH:$HOME/.brew/include\"
export CPLUS_INCLUDE_PATH=\"\$CPLUS_INCLUDE_PATH:$HOME/.brew/include\"

export HOMEBREW_CACHE=\"/tmp/brew_cache\"
export HOMEBREW_TEMP=\"/tmp/brew_temp\"
mkdir -p \"\$HOMEBREW_CACHE\" \"\$HOMEBREW_TEMP\"

if launchctl stop com.apple.rcd 2> /dev/null; then
	launchctl unload -w /System/Library/LaunchAgents/com.apple.rcd.plist
fi
" >> $INIT_FILE
fi

install_line "source \"$INIT_FILE\"" "$HOME/.bashrc"
install_line "source \"$INIT_FILE\"" "$HOME/.zshrc"
