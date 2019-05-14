# Xmodmap

cat > "$HOME/.xmodmap" <<EOF
clear control
clear mod4
clear lock

! Swap left ctrl with left command
keycode 133 = Control_L
keycode 134 = Control_R
keycode 37 = Super_L

! Eject = Delete
keycode 169 = Delete

! Caps lock = Control
! Shift+Caps lock = space
keycode 66 = Control_L space

! Accent key
keycode 94 = dead_acute dead_grave

! Space = Shift and Space (with xcape)
keycode 65 = Hyper_L
keycode 255 = space

add mod4 = Super_L
add control = Control_L Control_R
add shift = Hyper_L
EOF
