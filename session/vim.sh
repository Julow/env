# Vim setup

if ! which vim &>/dev/null; then
	exit 100
fi

VIM_DIR="$HOME/.vim"

mkdir -p "$VIM_DIR/"{,autoload,bundle}

define -a extra_rc

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
  local repo_name="${repo##*/}"
	if ! [[ -d "$dst" ]]; then
		echo "Install $repo_name"
		git clone -q "$repo" "$dst"
  else
    echo "Updating $repo_name"
    git -C "$dst" pull -q origin HEAD &
	fi
}

install_link ()
{
	local src="$1" dst="$VIM_DIR/bundle/$2"
	if [[ -d $src ]]; then
		echo "Link ${dst##*/}"
		ln -sfn "$src" "$dst"
	fi
}

# Install some packages

install_git "https://github.com/tpope/vim-fugitive"
install_git "https://github.com/tmux-plugins/vim-tmux-focus-events"
install_git "https://github.com/tpope/vim-surround"
install_git "https://github.com/tpope/vim-commentary"
install_git "https://github.com/roxma/vim-tmux-clipboard"
install_git "https://github.com/michaeljsmith/vim-indent-object"
install_git "https://github.com/vim-scripts/argtextobj.vim"
install_git "https://github.com/plasticboy/vim-markdown"
install_git "https://github.com/LnL7/vim-nix"

wait

install_link "$OPAM_SWITCH_PREFIX/share/ocp-indent/vim" "ocaml-ocp-indent"
extra_rc+=("let \$PATH = '$OPAM_SWITCH_PREFIX/bin:' . \$PATH")

# Vimrc

cp -r "$SESSION_DIR/vim"/{vimrc,ftplugin} "$VIM_DIR/"

for l in "${extra_rc[@]}"; do echo "$l"; done >> "$VIM_DIR/vimrc"
