# Vim setup

if ! which vim &>/dev/null; then
	exit 100
fi

PACKAGES=" \
	https://github.com/tpope/vim-fugitive
	https://github.com/tmux-plugins/vim-tmux-focus-events
	https://github.com/tpope/vim-surround
	https://github.com/tpope/vim-commentary
	https://github.com/roxma/vim-tmux-clipboard
	https://github.com/nelstrom/vim-markdown-folding
"

VIM_DIR="$HOME/.vim"

mkdir -p "$VIM_DIR/"{,autoload,bundle}

# Pathogen

PATHOGEN="$VIM_DIR/autoload/pathogen.vim"
if ! [[ -f "$PATHOGEN" ]]; then
	echo "Instal pathogen"
	curl -sLo "$PATHOGEN" https://raw.githubusercontent.com/tpope/vim-pathogen/master/autoload/pathogen.vim
fi

# Install some packages

for repo in $PACKAGES; do
	DST="$VIM_DIR/bundle/${repo##*/}"
	if ! [[ -d "$DST" ]]; then
		echo "Install ${repo##*/}"
		git clone -q "$repo" "$DST"
	fi
done

# Vimrc

cp "$SESSION_DIR/vimrc" "$HOME/.vimrc"
