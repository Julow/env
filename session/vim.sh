# Vim setup

if ! which vim &>/dev/null; then
	exit 100
fi

VIM_DIR="$HOME/.vim"
PACK_DIR="$VIM_DIR/pack/plugins/start"

mkdir -p "$PACK_DIR"

declare -a extra_rc

install_git ()
{
	local repo="$1"
	local dst="$PACK_DIR/${repo##*/}"
  local repo_name="${repo##*/}"
	if ! [[ -d "$dst" ]]; then
    local clone_args=()
    if [[ -n $2 ]]; then clone_args=(--branch "$2"); fi
		echo "Install $repo_name"
		git clone -q "${clone_args[@]}" "$repo" "$dst" &
  elif [[ $UPDATE -eq 1 ]]; then
    echo "Updating $repo_name"
    ( git -C "$dst" fetch -q origin;
      if [[ -n $2 ]]; then git -C "$dst" checkout "$2"; fi
      git -C "$dst" merge --ff-only ) &
	fi
}

install_link ()
{
	local src="$1" dst="$PACK_DIR/$2"
  local dst_dst=`readlink "$dst" 2>/dev/null || true`
	if [[ -d $src ]] && [[ "$dst_dst" != "$src" ]]; then
		echo "Link ${dst##*/}"
		ln -sfn "$src" "$dst"
	fi
}

# Install some packages

install_git "https://github.com/tpope/vim-fugitive"
install_git "https://github.com/tpope/vim-surround"
install_git "https://github.com/tpope/vim-commentary"
install_git "https://github.com/michaeljsmith/vim-indent-object"
install_git "https://github.com/vim-scripts/argtextobj.vim"
install_git "https://github.com/LnL7/vim-nix"
install_git "https://github.com/chiel92/vim-autoformat"
install_git "https://github.com/masukomi/vim-markdown-folding"
install_git "https://github.com/arp242/xdg_open.vim"
install_git "https://github.com/Julow/vim-tmux-focus-events" "any-term"

wait

install_link "$OPAM_SWITCH_PREFIX/share/ocp-indent/vim" "ocaml-ocp-indent"
extra_rc+=("let \$PATH = '$OPAM_SWITCH_PREFIX/bin:' . \$PATH")

if [[ $UPDATE -ne 1 ]]; then
  echo "To update, run with UPDATE=1"
fi

# Vimrc

cp -r "$SESSION_DIR/vim"/{vimrc,ftplugin} "$VIM_DIR/"

for l in "${extra_rc[@]}"; do echo; echo "$l"; done >> "$VIM_DIR/vimrc"
