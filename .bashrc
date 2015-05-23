#
# .bashrc
#
# https://github.com/Julow/My-Shell-Rc
#

#
#
#
export PATH="$HOME/.brew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
export LD_LIBRARY_PATH="$HOME/.brew/lib"
export SHELL="/bin/bash"
export PS1="$SHLVL> "

#
# Load aliases
#
if [ -f "$HOME/.bash_aliases" ]; then
	source "$HOME/.bash_aliases"
fi

#
# PS1
#

# Command status + SHLVL
function _ps1_status()
{
	STATUS="$?"
	if [[ "$SHLVL" -gt "1" ]]; then
		printf "$SHLVL> "
	fi
	if [[ "$STATUS" -eq "0" ]]; then
		printf "\033[32m$STATUS\033[0m"
	else
		printf "\033[31m$STATUS\033[0m"
	fi
};

# Git rev
function _ps1_git_rev()
{
	if [[ "$1" -gt "0" ]]; then
		printf "\033[31m%s\033[0m " "-$1"
	fi
	if [[ "$2" -gt "0" ]]; then
		printf "\033[32m%s\033[0m " "+$2"
	fi
};

# Git status
function _ps1_git()
{
	BRANCH=`git rev-parse --abbrev-ref HEAD 2> /dev/null` > /dev/null
	if [[ $? -eq 0 ]]; then
		if [[ ! "$BRANCH" == "master" ]]; then
			PRINT="[$BRANCH] "
		else
			PRINT=""
		fi
		STATUS=$(git status --porcelain)
		COLUM1=`echo "$STATUS" | cut -c 1-1`
		COLUM2=`echo "$STATUS" | cut -c 2-2`
		if [[ "$COLUM1" == *"A"* ]]; then
			PRINT=$PRINT"\033[32mA"
		fi
		if [[ "$COLUM1" == *"D"* ]]; then
			PRINT=$PRINT"\033[32mD"
		fi
		if [[ "$COLUM1" == *"M"* ]]; then
			PRINT=$PRINT"\033[32mM"
		fi
		if [[ "$COLUM1" == *"R"* ]]; then
			PRINT=$PRINT"\033[32mR"
		fi
		if [[ "$COLUM2" == *"D"* ]]; then
			PRINT=$PRINT"\033[31mD"
		fi
		if [[ "$COLUM2" == *"M"* ]]; then
			PRINT=$PRINT"\033[31mM"
		fi
		if [[ "$COLUM2" == *"?"* ]]; then
			PRINT=$PRINT"\033[31m?"
		fi
		if [[ "${#PRINT}" -gt "0" ]]; then
			printf "$PRINT\033[0m "
		fi
		_ps1_git_rev `git rev-list --left-right --count origin...HEAD 2> /dev/null || echo "0 0"`
	fi
};

# Bash
export PROMPT_COMMAND='export PS1="`_ps1_status` \033[36m\h \033[32m\w\033[0m `_ps1_git`"'

# Zsh
function precmd()
{
	export PROMPT="`_ps1_status` %F{cyan}%m %F{green}%~%f `_ps1_git`"
}

#
# Rc
#
alias rc="source $HOME/.bashrc"

return
