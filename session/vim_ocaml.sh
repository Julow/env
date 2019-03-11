# OCaml merlin plugin for Vim

OCPINDEX_PATH="$OPAM_SWITCH_PREFIX/share/ocp-index/vim"

if ! which vim &>/dev/null; then
	exit 100
fi

VIM_DIR="$HOME/.vim"

PLUGIN_PATH="$VIM_DIR/bundle/ocaml_ocp_index"

if [[ -d $OCPINDEX_PATH ]] && ! [[ -e $PLUGIN_PATH ]]; then
	ln -sf "$OCPINDEX_PATH" "$PLUGIN_PATH"
fi

mkdir -p "$VIM_DIR/ftplugin/"

cat > "$VIM_DIR/ftplugin/ocaml.vim" <<"EOF"
let no_plugin_maps = 1

setlocal commentstring=(*\ %s\ *)

call ocpindex#init()

nmap K :call ocpindex#print()<return>
EOF
