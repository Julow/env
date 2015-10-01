#
# prompt.sh
#
# Cool PS1 for bash and zsh
#

C_ESC="`printf '\033'`"

C_RESET=

C_RED=
C_GREEN=
C_YELLOW=
C_BLUE=
C_MAGENTA=
C_CYAN=
C_GRAY=

C_BLACK=
C_WHITE=

C_LRED=
C_LGREEN=
C_LYELLOW=
C_LBLUE=
C_LMAGENTA=
C_LCYAN=
C_LGRAY=

BG_RESET=

BG_RED=
BG_GREEN=
BG_YELLOW=
BG_BLUE=
BG_MAGENTA=
BG_CYAN=
BG_LGRAY=

BG_BLACK=
BG_WHITE=

BG_LRED=
BG_LGREEN=
BG_LYELLOW=
BG_LBLUE=
BG_LMAGENTA=
BG_LCYAN=
BG_GRAY=

function init_colors()
{
	PREFIX=""
	SUFFIX=""

	if [[ -n "$1" ]]
	then
		PREFIX="$1"
	fi

	if [[ -n "$2" ]]
	then
		SUFFIX="$2"
	fi

	C_RESET="${PREFIX}${C_ESC}[39m${SUFFIX}"

	C_RED="${PREFIX}${C_ESC}[31m${SUFFIX}"
	C_GREEN="${PREFIX}${C_ESC}[32m${SUFFIX}"
	C_YELLOW="${PREFIX}${C_ESC}[33m${SUFFIX}"
	C_BLUE="${PREFIX}${C_ESC}[34m${SUFFIX}"
	C_MAGENTA="${PREFIX}${C_ESC}[35m${SUFFIX}"
	C_CYAN="${PREFIX}${C_ESC}[36m${SUFFIX}"
	C_GRAY="${PREFIX}${C_ESC}[90m${SUFFIX}"

	C_BLACK="${PREFIX}${C_ESC}[30m${SUFFIX}"
	C_WHITE="${PREFIX}${C_ESC}[97m${SUFFIX}"

	C_LRED="${PREFIX}${C_ESC}[91m${SUFFIX}"
	C_LGREEN="${PREFIX}${C_ESC}[92m${SUFFIX}"
	C_LYELLOW="${PREFIX}${C_ESC}[93m${SUFFIX}"
	C_LBLUE="${PREFIX}${C_ESC}[94m${SUFFIX}"
	C_LMAGENTA="${PREFIX}${C_ESC}[95m${SUFFIX}"
	C_LCYAN="${PREFIX}${C_ESC}[96m${SUFFIX}"
	C_LGRAY="${PREFIX}${C_ESC}[37m${SUFFIX}"

	BG_RESET="${PREFIX}${C_ESC}[49m${SUFFIX}"

	BG_RED="${PREFIX}${C_ESC}[41m${SUFFIX}"
	BG_GREEN="${PREFIX}${C_ESC}[42m${SUFFIX}"
	BG_YELLOW="${PREFIX}${C_ESC}[43m${SUFFIX}"
	BG_BLUE="${PREFIX}${C_ESC}[44m${SUFFIX}"
	BG_MAGENTA="${PREFIX}${C_ESC}[45m${SUFFIX}"
	BG_CYAN="${PREFIX}${C_ESC}[46m${SUFFIX}"
	BG_LGRAY="${PREFIX}${C_ESC}[47m${SUFFIX}"

	BG_BLACK="${PREFIX}${C_ESC}[40m${SUFFIX}"
	BG_WHITE="${PREFIX}${C_ESC}[107m${SUFFIX}"

	BG_LRED="${PREFIX}${C_ESC}[101m${SUFFIX}"
	BG_LGREEN="${PREFIX}${C_ESC}[102m${SUFFIX}"
	BG_LYELLOW="${PREFIX}${C_ESC}[103m${SUFFIX}"
	BG_LBLUE="${PREFIX}${C_ESC}[104m${SUFFIX}"
	BG_LMAGENTA="${PREFIX}${C_ESC}[105m${SUFFIX}"
	BG_LCYAN="${PREFIX}${C_ESC}[106m${SUFFIX}"
	BG_GRAY="${PREFIX}${C_ESC}[100m${SUFFIX}"
};

if [[ -n $ZSH_VERSION ]]
then
	init_colors "%{" "%}"
else
	init_colors
fi

function _ps1_status()
{
	STATUS="$?"
	if [[ "$SHLVL" -gt "1" ]]; then
		printf "%s " "${C_GRAY}${SHLVL}>"
	fi
	if [[ ! "$STATUS" -eq "0" ]]; then
		printf "%s " "${C_RED}[${STATUS}]"
	fi
};

# Git rev
function _ps1_git_rev()
{
	if [[ "$1" -gt "0" ]]; then
		printf "%s" "${C_RED}-$1 "
	fi
	if [[ "$2" -gt "0" ]]; then
		printf "%s" "${C_GREEN}+$2 "
	fi
};

# Git status
function _ps1_git()
{
	BRANCH=`git rev-parse --abbrev-ref HEAD 2> /dev/null` > /dev/null
	if [[ $? -eq 0 ]]; then
		if [[ ! "$BRANCH" == "master" ]]; then
			PRINT="${C_MAGENTA}($BRANCH) "
		else
			PRINT=""
		fi
		STATUS=$(git status --porcelain)
		COLUM1=`echo "$STATUS" | cut -c 1-1`
		COLUM2=`echo "$STATUS" | cut -c 2-2`
		if [[ "$COLUM1" == *"A"* ]]; then
			PRINT=$PRINT"${C_GREEN}A"
		fi
		if [[ "$COLUM1" == *"D"* ]]; then
			PRINT=$PRINT"${C_GREEN}D"
		fi
		if [[ "$COLUM1" == *"M"* ]]; then
			PRINT=$PRINT"${C_GREEN}M"
		fi
		if [[ "$COLUM1" == *"R"* ]]; then
			PRINT=$PRINT"${C_GREEN}R"
		fi
		if [[ "$COLUM2" == *"D"* ]]; then
			PRINT=$PRINT"${C_RED}D"
		fi
		if [[ "$COLUM2" == *"M"* ]]; then
			PRINT=$PRINT"${C_RED}M"
		fi
		if [[ "$COLUM2" == *"?"* ]]; then
			PRINT=$PRINT"${C_RED}?"
		fi
		if [[ "${#PRINT}" -gt "0" ]]; then
			printf "%s " "$PRINT${C_RESET}"
		fi
		_ps1_git_rev `git rev-list --left-right --count origin...HEAD 2> /dev/null || echo "0 0"`
	fi
};

# Bash
export PROMPT_COMMAND='export PS1="`_ps1_status`${C_CYAN}\h ${C_LGREEN}\w `_ps1_git`${C_RESET}\n > "'

# Zsh
function precmd()
{
	export PROMPT="`_ps1_status`${C_CYAN}%m ${C_LGREEN}%~ `_ps1_git`${C_RESET}"$'\n'" > "
};

init_colors
