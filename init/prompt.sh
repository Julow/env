#
# prompt.sh
#
# Cool PS1 for bash and zsh
#

export C_RESET="\033[39m"

export C_RED="\033[31m"
export C_GREEN="\033[32m"
export C_YELLOW="\033[33m"
export C_BLUE="\033[34m"
export C_MAGENTA="\033[35m"
export C_CYAN="\033[36m"
export C_GRAY="\033[90m"

export C_BLACK="\033[30m"
export C_WHITE="\033[97m"

export C_LRED="\033[91m"
export C_LGREEN="\033[92m"
export C_LYELLOW="\033[93m"
export C_LBLUE="\033[94m"
export C_LMAGENTA="\033[95m"
export C_LCYAN="\033[96m"
export C_LGRAY="\033[37m"

export BG_RESET="\033[49m"

export BG_RED="\033[41m"
export BG_GREEN="\033[42m"
export BG_YELLOW="\033[43m"
export BG_BLUE="\033[44m"
export BG_MAGENTA="\033[45m"
export BG_CYAN="\033[46m"
export BG_LGRAY="\033[47m"

export BG_BLACK="\033[40m"
export BG_WHITE="\033[107m"

export BG_LRED="\033[101m"
export BG_LGREEN="\033[102m"
export BG_LYELLOW="\033[103m"
export BG_LBLUE="\033[104m"
export BG_LMAGENTA="\033[105m"
export BG_LCYAN="\033[106m"
export BG_GRAY="\033[100m"

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
# export PROMPT_COMMAND=""
# export PS1="${BG_LGRAY}${C_BLACK}2> ${C_RED} 1 ${BG_CYAN}${C_BLACK} juloo-pc "
# export PS1="$PS1${BG_LGREEN}${C_BLACK} $HOME ${BG_GREEN}"
# export PS1="$PS1${C_MAGENTA} (master) ${C_RED}M? ${C_RESET}${BG_RESET} "

# Zsh
function precmd()
{
	export PROMPT="`_ps1_status "%F{green}" "%F{red}" "%f"` %F{cyan}%m %F{green}%~%f `_ps1_git "%F{green}" "%F{red}" "%f"`"
};
