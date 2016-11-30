# opam

OPAM_DIR="$HOME/.opam"

if [[ "$1" == "?" ]] && ! [[ -d "$OPAM_DIR" ]]
then
	echo "Warning: $OPAM_DIR dir not found" >&2
	exit
fi

echo "$OPAM_DIR" >&2

echo "source $OPAM_DIR/opam-init/init.sh"
