# init rc

source "_utils.sh"

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
