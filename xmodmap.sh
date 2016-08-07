xmodmap - << XMODMAP_END
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
