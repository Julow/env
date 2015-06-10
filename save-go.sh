#
# Save Go
#
# Create 'bookmark' for working directories
#
# Can store unlimited save + one unamed save
#
# save [save name]
# (save a directory)
#
# go [save name]
# (cd to this directory)
#
# (save --help) for more commands
#

function save()
{
	if [[ "$1" == "-g" ]]; then
		cd "`save -i "$2"`"
	elif [[ "$1" == "-i" ]]; then
		if [[ -f ~/.save_go ]]; then
			cat ~/.save_go | grep -m 1 -i '^'"$2"'=' | cut -d '=' -f 2
		fi
	elif [[ "$1" == "-s" ]]; then
		if [[ -f ~/.save_go ]]; then
			if [[ "$2" == "" ]]; then
				_SAVE="`pwd`"
			else
				_SAVE="$2"
			fi
			cat ~/.save_go | grep -m 1 -i '='"$_SAVE"'$' | cut -d '=' -f 1
		fi
	elif [[ "$1" == "-l" ]]; then
		if [[ "$2" == "" ]]; then
			if [[ -f ~/.save_go ]]; then
				grep -v '^$' ~/.save_go
			fi
		else
			save -i "$2"
		fi
	elif [[ "$1" == "-r" ]]; then
		if [[ -f ~/.save_go ]]; then
			if [[ "$2" == "" ]]; then
				_SAVE="`save -s`"
			else
				_SAVE="$2"
			fi
			grep -iv '^'"$_SAVE"'=' ~/.save_go > ~/.save_go.tmp
			mv ~/.save_go.tmp ~/.save_go 2> /dev/null
		fi
	elif [[ "$1" == "--help" ]]; then
		save -h
	elif [[ "$1" == "-h" ]]; then
		echo "Save/Go"
		echo "    save -g <save>            Go to <save>"
		echo "    save -i <save>            Print the path of <save>"
		echo "    save -s                   Search the save with the current dir"
		echo "    save -s <dir>             Search the save with <dir>"
		echo "    save -l                   Print the list of saves"
		echo "    save -l <save>            Alias for 'save -i'"
		echo "    save -r                   Search and remove the save with the current dir"
		echo "    save -r <save>            Remove <save>"
		echo "    save -h"
		echo "    save --help               Print this message"
		echo
		echo "    save <save>               Create <save> with the current dir"
		echo "    go <save>                 Alias for 'save -g'"
		echo "    saved                     Alias for 'save -l'"
		echo
		echo "A save can have any name"
		echo "If <save> is blank, it refer to a save with no name."
		echo "All the saves are stored in '~/.save_go'"
	else
		save -r "$1"
		echo >> ~/.save_go
		echo -n "$1"'=' >> ~/.save_go
		pwd >> ~/.save_go
		grep -v '^$' ~/.save_go > ~/.save_go.tmp
		mv ~/.save_go.tmp ~/.save_go 2> /dev/null
	fi
};

alias go="save -g"
alias saved="save -l"
