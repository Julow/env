#
# .zshrc
#
# https://github.com/Julow/My-Shell-Rc
#

#
# Load base (aliases + bash prompt)
#
source "~/.bashrc"

#
# Zsh prompt
#
export PROMPT="%(?.%F{green}%?%f.%F{red}%?%f) %F{cyan}%m %F{green}%~%f $(_ps1_git '%F{green}' '%F{red}' '%f')"

if [[ "$SHLVL" -gt "1" ]]; then
	PROMPT="$SHLVL> "$PROMPT
fi

#
# Rc
#
alias rc="source ~/.zshrc"

#
# Clean
#
function _cleandir()
{
	if [[ "$1" != "" ]]; then
		if [[ "`ls -1 "$1" 2> /dev/null | wc -l`" -gt "0" ]]; then
			rm -rf "$1"/*
			rm -rf "$1"/.*
		fi
	fi
};

function clean()
{
	printf "\033[0;32mCrash reports\033[0;0m\n"
	_cleandir ~/Library/Logs/DiagnosticReports/
	printf "\033[0;32mApplications caches\033[0;0m\n"
	_cleandir ~/Library/Caches/
	_cleandir ~/.cache/
	printf "\033[0;32mTrash\033[0;0m\n"
	_cleandir ~/.Trash/
	printf "\033[0;32mRuby cache\033[0;0m\n"
	_cleandir ~/.rbx/
};

return
