#
# Extra Aliases
#
# s				Alias for subl (Sublime Text)
# ps			Override ps and print process on other ttys
# d				Diff with colors
# timeout		Execute a command and kill it after a timeout
# f				Recursive grep with colors (like ack)
#

#
# Sublime Text
#
# Open files/dirs in Sublime Text
# Open a new Sublime Text window if any
#
# s [file...]
#
# (Already exists on ubuntu: subl)
#
if [[ "`uname`" == "Darwin" ]]; then
	if [[ -f "/Applications/Sublime Text 2.app/Contents/SharedSupport/bin/subl" ]]; then
		alias subl="/Applications/Sublime\ Text\ 2.app/Contents/SharedSupport/bin/subl"
	elif [[ -f "/Applications/Sublime Text 3.app/Contents/SharedSupport/bin/subl" ]]; then
		alias subl="/Applications/Sublime\ Text\ 3.app/Contents/SharedSupport/bin/subl"
	elif [[ -f "/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl" ]]; then
		alias subl="/Applications/Sublime\ Text.app/Contents/SharedSupport/bin/subl"
	fi
fi

alias s="subl"

#
# PS
#
# Like ps but show process in other terminals
#
alias ps="ps -e -o 'pid %cpu %mem etime tty command' | grep -E ' ttys[0-9]+ | +COMMAND$| pts/[0-9]'"

#
# D
#
# Call the diff command and colorize the output
#
# d <file1> <file2>
#
function d()
{
	if [[ "$#" -lt 2 ]]; then
		echo "Error: d need 2 arguments"
	else
		printf "\033[0;32m$2\033[0;0m - \033[0;31m$1\033[0;0m\n"
		diff -- "$1" "$2" | sed -E "s/> (.*)|< (.*)/`printf "\033[0;32m"`\1`printf "\033[0;31m"`\2`printf "\033[0;0m"`/"
	fi
}

#
# Timeout
#
# Execute a command and kill it after a timeout
#
# timeout <cmd [args...]>
#
function timeout()
{
	$@ &
	PID=$!
	(sleep 10 ; kill $PID) &
	PID_SLEEP=$!
	wait $PID > /dev/null
	STATUS=$?
	if [[ $STATUS -gt 128 ]]; then
		kill $PID > /dev/null
	else
		kill $PID_SLEEP > /dev/null
	fi
};

#
# F
#
# Use grep to search in a directory
#
# f <search> [dir]
#
function f()
{
	if [[ "$2" == "" ]]; then
		F_DIRS="."
	else
		F_DIRS="$2"
	fi
	if [[ -t 1 ]]; then
		F_COLOR=always
	else
		F_COLOR=never
	fi
	grep -r -E --exclude-dir=".?*" --color="$F_COLOR" -C 3 "$1" "$F_DIRS"
};
