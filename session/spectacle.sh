# Spectacle preferences

SPECTACLE_DIR="$HOME/Library/Application Support/Spectacle"
SPECTACLE_SHORTCUTS="$SPECTACLE_DIR/Shortcuts.json"

if ! [[ -d $SPECTACLE_DIR ]]; then
	exit 100
fi

cat << EOF > "$SPECTACLE_SHORTCUTS"
[
  { "shortcut_name" : "RedoLastMove", "shortcut_key_binding" : null },
  { "shortcut_name" : "MakeSmaller", "shortcut_key_binding" : "alt+shift+-" },
  { "shortcut_name" : "MoveToPreviousThird", "shortcut_key_binding" : null },
  { "shortcut_name" : "MoveToUpperRight", "shortcut_key_binding" : null },
  { "shortcut_name" : "MoveToBottomHalf", "shortcut_key_binding" : null },
  { "shortcut_name" : "MoveToNextDisplay", "shortcut_key_binding" : "alt+shift+0" },
  { "shortcut_name" : "MoveToTopHalf", "shortcut_key_binding" : null },
  { "shortcut_name" : "MoveToLowerLeft", "shortcut_key_binding" : null },
  { "shortcut_name" : "MakeLarger", "shortcut_key_binding" : "alt+shift+=" },
  { "shortcut_name" : "UndoLastMove", "shortcut_key_binding" : "alt+shift+p" },
  { "shortcut_name" : "MoveToPreviousDisplay", "shortcut_key_binding" : "alt+9" },
  { "shortcut_name" : "MoveToFullscreen", "shortcut_key_binding" : "alt+shift+return" },
  { "shortcut_name" : "MoveToNextThird", "shortcut_key_binding" : null },
  { "shortcut_name" : "MoveToLeftHalf", "shortcut_key_binding" : "alt+shift+[" },
  { "shortcut_name" : "MoveToCenter", "shortcut_key_binding" : "alt+shift+delete" },
  { "shortcut_name" : "MoveToRightHalf", "shortcut_key_binding" : "alt+shift+]" },
  { "shortcut_name" : "MoveToLowerRight", "shortcut_key_binding" : null },
  { "shortcut_name" : "MoveToUpperLeft", "shortcut_key_binding" : null }
]
EOF
