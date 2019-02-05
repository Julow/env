# Vim setup

if ! which vim &>/dev/null; then
	exit 100
fi

PACKAGES=" \
	https://github.com/tpope/vim-fugitive \
	https://github.com/tmux-plugins/vim-tmux-focus-events"

VIM_DIR="$HOME/.vim"

mkdir -p "$VIM_DIR/"{,autoload,bundle}

RET=101

# Pathogen

PATHOGEN="$VIM_DIR/autoload/pathogen.vim"
if ! [[ -f "$PATHOGEN" ]]; then
	RET=0
	echo "Instal pathogen"
	curl -sLo "$PATHOGEN" https://raw.githubusercontent.com/tpope/vim-pathogen/master/autoload/pathogen.vim
fi

# Install some packages

for repo in $PACKAGES; do
	DST="$VIM_DIR/bundle/${repo##*/}"
	if ! [[ -d "$DST" ]]; then
		RET=0
		echo "Install ${repo##*/}"
		git clone -q "$repo" "$DST"
	fi
done

exit $RET
