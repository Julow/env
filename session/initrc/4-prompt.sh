C_ESC="`printf '\033'`"
PROMPT_BELL=`if [[ -n $TMUX ]]; then printf '\a'; fi`
PROMPT_CLEAR_TITLE=`printf '\033]0;\007'`

update_prompt ()
{
	local STATUS="$?"

	local C_RESET="\[${C_ESC}[39m\]"
	local C_RED="\[${C_ESC}[31m\]"
	local C_CYAN="\[${C_ESC}[36m\]"
	local C_GRAY="\[${C_ESC}[90m\]"
	local C_LGREEN="\[${C_ESC}[92m\]"
	# Used by git_status_line.sh
	local C_GREEN="\[${C_ESC}[32m\]"
	local C_YELLOW="\[${C_ESC}[33m\]"
	local C_MAGENTA="\[${C_ESC}[35m\]"

	local status_=""
	if [[ $STATUS -ne 0 ]]; then status_="$C_RED[$STATUS] "; fi
	local shlvl_=""
	if [[ $SHLVL -ge 1 ]]; then shlvl_="$C_GRAY$SHLVL> "; fi
	local meta_="\[$PROMPT_BELL$PROMPT_CLEAR_TITLE\]"

	export PS1="$meta_$shlvl_$status_$C_CYAN\h $C_LGREEN\w$C_RESET `. git_status_line.sh`"
}

export PROMPT_COMMAND=update_prompt
