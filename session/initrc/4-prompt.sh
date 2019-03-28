C_ESC="`printf '\033'`"

C_RESET="${C_ESC}[39m${SUFFIX}"
C_RED="${C_ESC}[31m${SUFFIX}"
C_CYAN="${C_ESC}[36m${SUFFIX}"
C_GRAY="${C_ESC}[90m${SUFFIX}"
C_LGREEN="${C_ESC}[92m${SUFFIX}"

function _ps1_status()
{
	STATUS="$?"
	if [[ $STATUS -ne 0 ]]; then
		echo -n "${C_RED}[${STATUS}] "
	fi
};

SHLVL=""
if [[ "$SHLVL" -gt "1" ]]; then SHLVL="${C_GRAY}${SHLVL}> "; fi

CLEAR_TITLE=`printf '\033]0;\007'`
BELL=`if [[ -n $TMUX ]]; then printf '\a'; fi`

# Bash
export PROMPT_COMMAND='export PS1="$BELL$CLEAR_TITLE$SHLVL`_ps1_status`${C_CYAN}\h ${C_LGREEN}\w${C_RESET} "'
