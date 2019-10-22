set -e

for rc in "$@"; do
	echo "# ${rc##*/}"
	if [[ $rc = *.gen.sh ]]
	then bash -e "$rc"
	else cat "$rc"
	fi
	echo
done
