#
# prompt.sh
#
# Cool PS1 for bash and zsh
#

export C_ESC="`printf '\033'`"

if [[ -n $ZSH_VERSION ]]
then
	export C_RESET="%{${C_ESC}[39m%}"

	export C_RED="%{${C_ESC}[31m%}"
	export C_GREEN="%{${C_ESC}[32m%}"
	export C_YELLOW="%{${C_ESC}[33m%}"
	export C_BLUE="%{${C_ESC}[34m%}"
	export C_MAGENTA="%{${C_ESC}[35m%}"
	export C_CYAN="%{${C_ESC}[36m%}"
	export C_GRAY="%{${C_ESC}[90m%}"

	export C_BLACK="%{${C_ESC}[30m%}"
	export C_WHITE="%{${C_ESC}[97m%}"

	export C_LRED="%{${C_ESC}[91m%}"
	export C_LGREEN="%{${C_ESC}[92m%}"
	export C_LYELLOW="%{${C_ESC}[93m%}"
	export C_LBLUE="%{${C_ESC}[94m%}"
	export C_LMAGENTA="%{${C_ESC}[95m%}"
	export C_LCYAN="%{${C_ESC}[96m%}"
	export C_LGRAY="%{${C_ESC}[37m%}"

	export BG_RESET="%{${C_ESC}[49m%}"

	export BG_RED="%{${C_ESC}[41m%}"
	export BG_GREEN="%{${C_ESC}[42m%}"
	export BG_YELLOW="%{${C_ESC}[43m%}"
	export BG_BLUE="%{${C_ESC}[44m%}"
	export BG_MAGENTA="%{${C_ESC}[45m%}"
	export BG_CYAN="%{${C_ESC}[46m%}"
	export BG_LGRAY="%{${C_ESC}[47m%}"

	export BG_BLACK="%{${C_ESC}[40m%}"
	export BG_WHITE="%{${C_ESC}[107m%}"

	export BG_LRED="%{${C_ESC}[101m%}"
	export BG_LGREEN="%{${C_ESC}[102m%}"
	export BG_LYELLOW="%{${C_ESC}[103m%}"
	export BG_LBLUE="%{${C_ESC}[104m%}"
	export BG_LMAGENTA="%{${C_ESC}[105m%}"
	export BG_LCYAN="%{${C_ESC}[106m%}"
	export BG_GRAY="%{${C_ESC}[100m%}"
else
	export C_RESET="${C_ESC}[39m"

	export C_RED="${C_ESC}[31m"
	export C_GREEN="${C_ESC}[32m"
	export C_YELLOW="${C_ESC}[33m"
	export C_BLUE="${C_ESC}[34m"
	export C_MAGENTA="${C_ESC}[35m"
	export C_CYAN="${C_ESC}[36m"
	export C_GRAY="${C_ESC}[90m"

	export C_BLACK="${C_ESC}[30m"
	export C_WHITE="${C_ESC}[97m"

	export C_LRED="${C_ESC}[91m"
	export C_LGREEN="${C_ESC}[92m"
	export C_LYELLOW="${C_ESC}[93m"
	export C_LBLUE="${C_ESC}[94m"
	export C_LMAGENTA="${C_ESC}[95m"
	export C_LCYAN="${C_ESC}[96m"
	export C_LGRAY="${C_ESC}[37m"

	export BG_RESET="${C_ESC}[49m"

	export BG_RED="${C_ESC}[41m"
	export BG_GREEN="${C_ESC}[42m"
	export BG_YELLOW="${C_ESC}[43m"
	export BG_BLUE="${C_ESC}[44m"
	export BG_MAGENTA="${C_ESC}[45m"
	export BG_CYAN="${C_ESC}[46m"
	export BG_LGRAY="${C_ESC}[47m"

	export BG_BLACK="${C_ESC}[40m"
	export BG_WHITE="${C_ESC}[107m"

	export BG_LRED="${C_ESC}[101m"
	export BG_LGREEN="${C_ESC}[102m"
	export BG_LYELLOW="${C_ESC}[103m"
	export BG_LBLUE="${C_ESC}[104m"
	export BG_LMAGENTA="${C_ESC}[105m"
	export BG_LCYAN="${C_ESC}[106m"
	export BG_GRAY="${C_ESC}[100m"
	# export C_RESET="\033[39m"

	# export C_RED="\033[31m"
	# export C_GREEN="\033[32m"
	# export C_YELLOW="\033[33m"
	# export C_BLUE="\033[34m"
	# export C_MAGENTA="\033[35m"
	# export C_CYAN="\033[36m"
	# export C_GRAY="\033[90m"

	# export C_BLACK="\033[30m"
	# export C_WHITE="\033[97m"

	# export C_LRED="\033[91m"
	# export C_LGREEN="\033[92m"
	# export C_LYELLOW="\033[93m"
	# export C_LBLUE="\033[94m"
	# export C_LMAGENTA="\033[95m"
	# export C_LCYAN="\033[96m"
	# export C_LGRAY="\033[37m"

	# export BG_RESET="\033[49m"

	# export BG_RED="\033[41m"
	# export BG_GREEN="\033[42m"
	# export BG_YELLOW="\033[43m"
	# export BG_BLUE="\033[44m"
	# export BG_MAGENTA="\033[45m"
	# export BG_CYAN="\033[46m"
	# export BG_LGRAY="\033[47m"

	# export BG_BLACK="\033[40m"
	# export BG_WHITE="\033[107m"

	# export BG_LRED="\033[101m"
	# export BG_LGREEN="\033[102m"
	# export BG_LYELLOW="\033[103m"
	# export BG_LBLUE="\033[104m"
	# export BG_LMAGENTA="\033[105m"
	# export BG_LCYAN="\033[106m"
	# export BG_GRAY="\033[100m"
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

# function _ps1_test()
# {
# 	_PS1="`_ps1_status`${C_CYAN}$HOSTNAME ${C_LGREEN}`pwd` `_ps1_git`${C_RESET}"
# 	_PS1_CLEAN="`printf "%s" "$_PS1" | sed -E 's/'"${C_ESC}"'\[[0-9;]+m//g'`"
# 	printf '\r'
# 	tput cuf $((`tput cols` - ${#_PS1_CLEAN}))
# 	printf "%s" "$_PS1"
# 	printf '\r\$ > '
# }

# Bash
# export PROMPT_COMMAND='export PS1="`_ps1_test`"'
export PROMPT_COMMAND='export PS1="`_ps1_status`${C_CYAN}\h ${C_LGREEN}\w `_ps1_git`${C_RESET}\n > "'

# Zsh
function precmd()
{
	export PROMPT="`_ps1_status`${C_CYAN}%m ${C_LGREEN}%~ `_ps1_git`${C_RESET}"$'\n'" > "
};
