# opam

OPAM_DIR="$HOME/.opam"

if [[ "$1" == "?" ]] && ! [[ -d "$OPAM_DIR" ]]; then exit; fi

echo "source $OPAM_DIR/opam-init/init.sh"
