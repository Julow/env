# init rc
# Put the path to the $INIT_FILE into any file and try to avoid dupplicates
# Takes a list of files as arguments
# Arguments can ends with '?',
#  meaning that the argument will be ignored if the file does not exists

function install_line()
{
	if ! grep "$1" "$2" > /dev/null 2> /dev/null; then
		echo "Update $2"
		echo "
$1" >> "$2"
	fi
}

for f in $@; do
	if [[ ! "$f" == "/"* ]]; then
		f="$HOME/$f"
	fi
	if [[ "$f" == *"?" ]]; then
		f=${f%${f##*[!?]}}
		if [[ ! -f "$f" ]]; then continue; fi
	fi
	install_line "source \"$INIT_FILE\"" "$f" >&2
done
