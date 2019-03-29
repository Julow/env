C_ESC="`printf '\033'`"

C_RESET="\[${C_ESC}[39m\]"
C_RED="\[${C_ESC}[31m\]"
C_CYAN="\[${C_ESC}[36m\]"
C_GRAY="\[${C_ESC}[90m\]"
C_LGREEN="\[${C_ESC}[92m\]"

function _ps1_status()
{
	status="$?"
	if [[ $status -ne 0 ]]; then
		echo -n "$C_RED[$status] "
	fi
};

SHLVL_=""
if [[ $SHLVL -ge 1 ]]; then SHLVL_="$C_GRAY$SHLVL> "; fi

CLEAR_TITLE=`printf '\033]0;\007'`
BELL=`if [[ -n $TMUX ]]; then printf '\a'; fi`

# Bash
export PROMPT_COMMAND='export PS1="\[$BELL$CLEAR_TITLE\]$SHLVL_`_ps1_status`$C_CYAN\h $C_LGREEN\w$C_RESET "'
