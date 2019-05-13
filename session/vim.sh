# Vim setup

if ! which vim &>/dev/null; then
	exit 100
fi

VIM_DIR="$HOME/.vim"

mkdir -p "$VIM_DIR/"{,autoload,bundle}

# Pathogen

PATHOGEN="$VIM_DIR/autoload/pathogen.vim"
if ! [[ -f "$PATHOGEN" ]]; then
	echo "Instal pathogen"
	curl -sLo "$PATHOGEN" https://raw.githubusercontent.com/tpope/vim-pathogen/master/autoload/pathogen.vim
fi

install_git ()
{
	local repo="$1"
	local dst="$VIM_DIR/bundle/${repo##*/}"
	if ! [[ -d "$dst" ]]; then
		echo "Install ${repo##*/}"
		git clone -q "$repo" "$dst"
	fi
}

install_link ()
{
	local src="$1" dst="$VIM_DIR/bundle/$2"
	if [[ -d $src ]] && ! [[ -e $dst ]]; then
		echo "Link $dst"
		ln -sf "$src" "$dst"
	fi
}

# Install some packages

install_git "https://github.com/tpope/vim-fugitive"
install_git "https://github.com/tmux-plugins/vim-tmux-focus-events"
install_git "https://github.com/tpope/vim-surround"
install_git "https://github.com/tpope/vim-commentary"
install_git "https://github.com/roxma/vim-tmux-clipboard"
install_git "https://github.com/nelstrom/vim-markdown-folding"
install_git "https://github.com/michaeljsmith/vim-indent-object"
install_git "https://github.com/vim-scripts/argtextobj.vim"

install_link "$OPAM_SWITCH_PREFIX/share/ocp-index/vim" "ocaml-ocp-index"
install_link "$OPAM_SWITCH_PREFIX/share/ocp-indent/vim" "ocaml-ocp-indent"

# Vimrc

cp -r "$SESSION_DIR/vim"/{vimrc,ftplugin} "$VIM_DIR/"
