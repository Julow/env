# init brew

DEFAULT_BREW_DIR="$HOME/.brew"

BREW_DIR="$1"

if [[ ! "$BREW_DIR" == "/"* ]]; then BREW_DIR="$HOME/$BREW_DIR"; fi
if [[ "$BREW_DIR" == *"?" ]]; then
	BREW_DIR=${BREW_DIR%${BREW_DIR##*[!?]}}
	if [[ ! -d "$BREW_DIR" ]]; then
		echo "Warning: $BREW_DIR dir not found" >&2
		exit
	fi
fi

if [[ -z "$BREW_DIR" ]]; then BREW_DIR="$DEFAULT_BREW_DIR"; fi

echo "export PATH=\"\$HOME/.brew/bin:\$PATH\"
export LIBRARY_PATH=\"\$LIBRARY_PATH:\$HOME/.brew/lib\"
export LD_LIBRARY_PATH=\"\$LD_LIBRARY_PATH:\$HOME/.brew/lib\"
export C_INCLUDE_PATH=\"\$C_INCLUDE_PATH:\$HOME/.brew/include\"
export CPLUS_INCLUDE_PATH=\"\$CPLUS_INCLUDE_PATH:\$HOME/.brew/include\"

export HOMEBREW_CACHE=\"/tmp/brew_cache\"
export HOMEBREW_TEMP=\"/tmp/brew_temp\"
mkdir -p \"\$HOMEBREW_CACHE\" \"\$HOMEBREW_TEMP\"
"
