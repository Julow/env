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
keycode 66 = Control_L

! Accent key
keycode 94 = dead_acute dead_grave

add mod4 = Super_L
add control = Control_L Control_R
EOF
