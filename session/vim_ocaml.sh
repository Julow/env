# OCaml merlin plugin for Vim

if ! which vim &>/dev/null; then
	exit 100
fi

VIM_DIR="$HOME/.vim"

link_plugin ()
{
	SRC_PATH=$1
	DST_PATH="$VIM_DIR/bundle/$2"
	if [[ -d $SRC_PATH ]] && ! [[ -e $DST_PATH ]]; then
		echo "Link $DST_PATH"
		ln -sf "$SRC_PATH" "$DST_PATH"
	fi
}

link_plugin "$OPAM_SWITCH_PREFIX/share/ocp-index/vim" "ocaml-ocp-index"
link_plugin "$OPAM_SWITCH_PREFIX/share/ocp-indent/vim" "ocaml-ocp-indent"

mkdir -p "$VIM_DIR/ftplugin/"

cat > "$VIM_DIR/ftplugin/ocaml.vim" <<"EOF"
let no_plugin_maps = 1

setlocal commentstring=(*\ %s\ *)

nmap K :call ocpindex#print()<return>
EOF
