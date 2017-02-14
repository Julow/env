xkbcomp -I$HOME/_env/xkb - $DISPLAY << KEYMAP_END
xkb_keymap {
	xkb_keycodes	{ include "evdev+aliases(qwerty)" };
	xkb_types		{ include "complete" };
	xkb_compat		{ include "complete" };
	xkb_symbols		{ include "pc+us+inet(evdev)+juloo(custom)" };
	xkb_geometry	{ include "pc(pc105)" };
};
KEYMAP_END
