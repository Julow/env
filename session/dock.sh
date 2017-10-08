# Dock

DOCK_PREFERENCES="$HOME/Library/Preferences/com.apple.dock.plist"

if ! [[ -e $DOCK_PREFERENCES ]]; then
	exit 100
fi

cp "$SESSION_DIR/dock.plist" "$DOCK_PREFERENCES"
