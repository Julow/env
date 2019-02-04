# OCaml merlin plugin for Vim

MERLIN_PATH="$OPAM_SWITCH_PREFIX/share/merlin/vim"

if ! which vim &>/dev/null; then
	exit 100
fi

VIM_DIR="$HOME/.vim"

PLUGIN_PATH="$VIM_DIR/bundle/ocaml_merlin"

if [[ -d $MERLIN_PATH ]] && ! [[ -e $PLUGIN_PATH ]]; then
	ln -sf "$MERLIN_PATH" "$PLUGIN_PATH"
fi

mkdir -p "$VIM_DIR/ftplugin/"

cat > "$VIM_DIR/ftplugin/ocaml.vim" <<"EOF"
" Clear merlin highlighting
autocmd User ClearHighlight :MerlinClearEnclosing

let no_plugin_maps = 1
EOF
