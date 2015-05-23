#
# .bashrc
#
# https://github.com/Julow/My-Shell-Rc
#

#
# Load aliases
#
if [ -f "~/.bash_aliases" ]; then
	source "~/.bash_aliases"
fi

#
# PS1
#
function _ps1_status()
{
	STATUS="$?"
	if [[ "$SHLVL" -gt "1" ]]; then
		printf "$SHLVL> "
	fi
	if [[ "$STATUS" -eq "0" ]]; then
		printf "\033[32m$STATUS"
	else
		printf "\033[31m$STATUS"
	fi
};

function _ps1_git_rev()
{
	if [[ "$4" -gt "0" ]]; then
		printf "%s " "$2-$4$3"
	fi
	if [[ "$5" -gt "0" ]]; then
		printf "%s " "$1+$5$3"
	fi
};

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

function _ps1()
{
	export PS1="`_ps1_status` \033[36m\h \033[32m\w\033[0m `_ps1_git '\033[32m' '\033[31m' '\033[0m'`"
};

export PROMPT_COMMAND="_ps1"

#
# Rc
#
alias rc="source ~/.bashrc"
