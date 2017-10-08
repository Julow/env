# Spectacle preferences

SPECTACLE_DIR="$HOME/Library/Application Support/Spectacle"
SPECTACLE_SHORTCUTS="$SPECTACLE_DIR/Shortcuts.json"
SPECTACLE_PREFERENCES="$HOME/Library/Preferences/com.divisiblebyzero.Spectacle.plist"

if ! [[ -d $SPECTACLE_DIR ]]; then
	exit 100
fi

cp "$SESSION_DIR/spectacle.plist" "$SPECTACLE_PREFERENCES"

cat << EOF > "$SPECTACLE_SHORTCUTS"
[
	{ "shortcut_key_binding": "alt+cmd+keypaddecimal", "shortcut_name": "MoveToPreviousThird" },
	{ "shortcut_key_binding": "alt+cmd+keypadminus", "shortcut_name": "MakeSmaller" },
	{ "shortcut_key_binding": null, "shortcut_name": "RedoLastMove" },
	{ "shortcut_key_binding": "alt+cmd+keypad9", "shortcut_name": "MoveToUpperRight" },
	{ "shortcut_key_binding": "alt+cmd+keypad2", "shortcut_name": "MoveToBottomHalf" },
	{ "shortcut_key_binding": "alt+cmd+keypadmultiply", "shortcut_name": "MoveToNextDisplay" },
	{ "shortcut_key_binding": "alt+cmd+keypad8", "shortcut_name": "MoveToTopHalf" },
	{ "shortcut_key_binding": "alt+cmd+keypad1", "shortcut_name": "MoveToLowerLeft" },
	{ "shortcut_key_binding": "alt+cmd+keypadplus", "shortcut_name": "MakeLarger" },
	{ "shortcut_key_binding": "alt+cmd+keypaddivide", "shortcut_name": "MoveToPreviousDisplay" },
	{ "shortcut_key_binding": null, "shortcut_name": "UndoLastMove" },
	{ "shortcut_key_binding": "alt+cmd+keypad5", "shortcut_name": "MoveToFullscreen" },
	{ "shortcut_key_binding": "alt+cmd+keypad0", "shortcut_name": "MoveToNextThird" },
	{ "shortcut_key_binding": "alt+cmd+keypad4", "shortcut_name": "MoveToLeftHalf" },
	{ "shortcut_key_binding": "alt+cmd+keypadequals", "shortcut_name": "MoveToCenter" },
	{ "shortcut_key_binding": "alt+cmd+keypad6", "shortcut_name": "MoveToRightHalf" },
	{ "shortcut_key_binding": "alt+cmd+keypad3", "shortcut_name": "MoveToLowerRight" },
	{ "shortcut_key_binding": "alt+cmd+keypad7", "shortcut_name": "MoveToUpperLeft" }
]
EOF
