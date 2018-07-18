BREW_DIR="$HOME/.brew"

if [[ ! -d "$BREW_DIR" ]]; then exit; fi

echo "export PATH=\"$BREW_DIR/bin:\$PATH\"
export LIBRARY_PATH=\"\$LIBRARY_PATH:$BREW_DIR/lib\"
export LD_LIBRARY_PATH=\"\$LD_LIBRARY_PATH:$BREW_DIR/lib\"
export C_INCLUDE_PATH=\"\$C_INCLUDE_PATH:$BREW_DIR/include\"
export CPLUS_INCLUDE_PATH=\"\$CPLUS_INCLUDE_PATH:$BREW_DIR/include\"

export HOMEBREW_CACHE=\"/tmp/brew_cache\"
export HOMEBREW_TEMP=\"/tmp/brew_temp\"
export HOMEBREW_LOCKS=\"/tmp/brew_locks\"
mkdir -p \"\$HOMEBREW_CACHE\" \"\$HOMEBREW_TEMP\" \"\$HOMEBREW_LOCKS\"

rm -rf $BREW_DIR/Library/Locks
ln -sf \$HOMEBREW_LOCKS $BREW_DIR/Library/Locks
"
