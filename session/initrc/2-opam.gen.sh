# opam

OPAM_INIT="$HOME/.opam/opam-init/init.sh"

if ! [[ -d "$OPAM_INIT" ]]; then exit; fi

echo "source \"$OPAM_INIT\""
