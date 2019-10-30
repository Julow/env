# Control sequences
PROMPT_META=""
if [[ -n $TMUX ]]; then PROMPT_META+=`printf '\a'`; fi
PROMPT_META+=`printf '\033]0;\007'` # Clean title

C_ESC="`printf '\033'`"
C_RESET="\[${C_ESC}[39m\]"
C_RED="\[${C_ESC}[31m\]"
C_CYAN="\[${C_ESC}[36m\]"
C_GRAY="\[${C_ESC}[90m\]"
C_LGREEN="\[${C_ESC}[92m\]"
# Used by git_status_line.sh
C_GREEN="\[${C_ESC}[32m\]"
C_YELLOW="\[${C_ESC}[33m\]"
C_MAGENTA="\[${C_ESC}[35m\]"

PROMPT_HOSTNAME="\h "
PROMPT_PWD="\w"

# $WORKSPACE replaces the hostname and pwd is relative to $WORKSPACE_ROOT
if [[ -n $WORKSPACE ]]; then
  PROMPT_HOSTNAME="$WORKSPACE";
  PROMPT_PWD=" $PROMPT_PWD"
  if [[ -n $WORKSPACE_ROOT ]]; then PROMPT_PWD="\${PWD#'$WORKSPACE_ROOT'}"; fi
fi

# SHLVL
PROMPT_SHLVL=""
if [[ $SHLVL -gt 1 ]]; then PROMPT_SHLVL="$C_GRAY$SHLVL> "; fi

update_prompt ()
{
  local status="$?"
  local PROMPT_STATUS=""
  if [[ $status -ne 0 ]]; then PROMPT_STATUS="$C_RED[$status] "; fi
  local PROMPT_GIT=`. git_status_line.sh`
  PS1="\[$PROMPT_META\]$PROMPT_SHLVL$PROMPT_STATUS$C_CYAN$PROMPT_HOSTNAME$C_LGREEN$PROMPT_PWD$C_RESET $PROMPT_GIT"
}

export PROMPT_COMMAND=update_prompt
