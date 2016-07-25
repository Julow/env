# init xmodmap

source "_utils.sh"

XINITRC_FILE="$HOME/.xinitrc"
XMODMAP_FILE="$HOME/.Xmodmap"

install_line "xmodmap ~/.Xmodmap" "$XINITRC_FILE" >&2

if [[ -f "$XMODMAP_FILE" ]]; then
	OLD_XMODMAP="/tmp/.Xmodmap.old"
	echo "$XMODMAP_FILE already exists, save it to $OLD_XMODMAP" >&2
	mv "$XMODMAP_FILE" "$OLD_XMODMAP"
fi

cat << XMODMAP_END > "$XMODMAP_FILE"
clear control
clear mod4
clear lock

! Swap left ctrl with left command
keycode 133 = Control_L NoSymbol Control_L
keycode 134 = Super_R NoSymbol Super_R
keycode 37 = Super_L NoSymbol Super_L
add control = Control_L
add mod4 = Super_L
add mod4 = Super_R

! Eject = Delete
keycode 169 = Delete

! Caps lock = Control
keycode 66 = Control_L
add control = Control_L
XMODMAP_END
