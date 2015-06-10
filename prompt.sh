#
# prompt.sh
#
# Cool PS1 for bash and zsh
#
# Show:
#  SHLVL (if > 1 only)
#  Last command status
#  Hostname
#  Current working directory
#  Git status (including untracked)
#  Git revisions (+ and -)
#

function _ps1_status()
{
	STATUS="$?"
	if [[ "$SHLVL" -gt "1" ]]; then
		printf "%s" "$SHLVL> "
	fi
	if [[ "$STATUS" -eq "0" ]]; then
		printf "%s" "$1$STATUS$3"
	else
		printf "%s" "$2$STATUS$3"
	fi
};

# Git rev
function _ps1_git_rev()
{
	if [[ "$4" -gt "0" ]]; then
		printf "%s" "$2-$4$3 "
	fi
	if [[ "$5" -gt "0" ]]; then
		printf "%s" "$1+$5$3 "
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
			PRINT=$PRINT"$1A"
		fi
		if [[ "$COLUM1" == *"D"* ]]; then
			PRINT=$PRINT"$1D"
		fi
		if [[ "$COLUM1" == *"M"* ]]; then
			PRINT=$PRINT"$1M"
		fi
		if [[ "$COLUM1" == *"R"* ]]; then
			PRINT=$PRINT"$1R"
		fi
		if [[ "$COLUM2" == *"D"* ]]; then
			PRINT=$PRINT"$2D"
		fi
		if [[ "$COLUM2" == *"M"* ]]; then
			PRINT=$PRINT"$2M"
		fi
		if [[ "$COLUM2" == *"?"* ]]; then
			PRINT=$PRINT"$2?"
		fi
		if [[ "${#PRINT}" -gt "0" ]]; then
			printf "%s " "$PRINT$3"
		fi
		_ps1_git_rev "$1" "$2" "$3" `git rev-list --left-right --count origin...HEAD 2> /dev/null || echo "0 0"`
	fi
};

# Bash
export PROMPT_COMMAND='export PS1="`_ps1_status "\033[32m" "\033[31m" "\033[0m"` \033[36m\h \033[32m\w\033[0m `_ps1_git "\033[32m" "\033[31m" "\033[0m"`"'

# Zsh
function precmd()
{
	export PROMPT="`_ps1_status "%F{green}" "%F{red}" "%f"` %F{cyan}%m %F{green}%~%f `_ps1_git "%F{green}" "%F{red}" "%f"`"
};
