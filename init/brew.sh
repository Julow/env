# init brew

DEFAULT_BREW_DIR="$HOME/.brew"

BREW_DIR="$1"

if [[ "$BREW_DIR" == *"?" ]]; then
	BREW_DIR=${BREW_DIR%${BREW_DIR##*[!?]}}
	OPTIONAL=1
else
	OPTIONAL=0
fi

if [[ -z "$BREW_DIR" ]]; then BREW_DIR="$DEFAULT_BREW_DIR";
elif [[ ! "$BREW_DIR" == "/"* ]]; then BREW_DIR="$HOME/$BREW_DIR"; fi

if [[ ! -d "$BREW_DIR" ]] && [[ "$OPTIONAL" -eq 1 ]]; then
	echo "Warning: $BREW_DIR dir not found" >&2
	exit
fi

echo "$BREW_DIR" >&2

echo "export PATH=\"\$HOME/.brew/bin:\$PATH\"
export LIBRARY_PATH=\"\$LIBRARY_PATH:\$HOME/.brew/lib\"
export LD_LIBRARY_PATH=\"\$LD_LIBRARY_PATH:\$HOME/.brew/lib\"
export C_INCLUDE_PATH=\"\$C_INCLUDE_PATH:\$HOME/.brew/include\"
export CPLUS_INCLUDE_PATH=\"\$CPLUS_INCLUDE_PATH:\$HOME/.brew/include\"

export HOMEBREW_CACHE=\"/tmp/brew_cache\"
export HOMEBREW_TEMP=\"/tmp/brew_temp\"
export HOMEBREW_LOCKS=\"/tmp/brew_locks\"
mkdir -p \"\$HOMEBREW_CACHE\" \"\$HOMEBREW_TEMP\" \"\$HOMEBREW_LOCKS\"

rm -rf \$HOME/.brew/Library/Locks
ln -sf \$HOMEBREW_LOCKS \$HOME/.brew/Library/Locks
"
