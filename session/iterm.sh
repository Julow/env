# iTerm preferences

ITERM_PREFERENCES="$HOME/Library/Preferences/com.googlecode.iterm2.plist"

if ! [[ -e $ITERM_PREFERENCES ]]; then
	exit 100
fi

cp "$SESSION_DIR/iterm.plist" "$ITERM_PREFERENCES"
