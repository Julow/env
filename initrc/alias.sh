# init aliases

ALIAS_DIR="$INIT_DIR/aliases"

if [[ "$#" -eq 0 ]]; then
	ALIASES=`basename "$ALIAS_DIR"/*`
	ALIASES=${ALIASES//.sh/}
	ALIASES=${ALIASES//
/ }
	echo "Init all alias: ${ALIASES}" >&2
else
	ALIASES="$@"
fi

echo "Aliases: $ALIASES" >&2

for alias in $ALIASES; do
	F="$ALIAS_DIR/$alias.sh"
	if [[ -f "$F" ]]; then
		cat "$F"
		echo
	else
		echo "Warning: Invalid alias: $alias" >&2
	fi
done
