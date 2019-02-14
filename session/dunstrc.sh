# Dunst config

if ! which dunst >/dev/null; then
	exit 100
fi

DUNST_PATH="$HOME/.config/dunst"

mkdir -p "$DUNST_PATH"
cp "$SESSION_DIR/dunstrc" "$DUNST_PATH/dunstrc"
