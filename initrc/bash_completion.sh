# Enable bash_completion if present
# bash_completion is not sourced if the shell is in posix mode

if [[ -f /usr/share/bash-completion/bash_completion ]]; then
	COMP_PATH=/usr/share/bash-completion/bash_completion
elif [[ -f /etc/bash_completion ]]; then
	COMP_PATH=/etc/bash_completion
fi

echo 'if ! shopt -oq posix; then . '$COMP_PATH'; fi'
