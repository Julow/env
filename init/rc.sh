# init rc

function install_line()
{
	if grep "$1" "$2" > /dev/null 2> /dev/null; then
		echo "$2 already up to date"
	else
		echo "Update $2"
		echo >> $2
		echo "$1" >> $2
	fi
}

for f in $@; do
	if [[ ! "$f" == "/"* ]]; then
		f="$HOME/$f"
	fi
	if [[ "$f" == *"?" ]]; then
		f=${f%${f##*[!?]}}
		if [[ ! -f "$f" ]]; then
			echo "Warning: $f file not found" >&2
			continue
		fi
	fi
	install_line "source \"$INIT_FILE\"" "$f" >&2
done
