# Markdown

if ! which vim &>/dev/null; then
	exit 100
fi

VIM_DIR="$HOME/.vim"

mkdir -p "$VIM_DIR/ftplugin/"

cat > "$VIM_DIR/ftplugin/markdown.vim" <<"EOF"
let g:markdown_fold_style = 'nested'
set foldlevel=1
EOF
