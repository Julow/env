all::
.PHONY: all

INSTALL =

# Xmonad

XMONAD_HS = $(HOME)/.xmonad/xmonad.hs

$(XMONAD_HS): xmonad/xmonad.hs
	cp "$^" "$@"
	xmonad --recompile

all:: $(XMONAD_HS)

# X

XINITRC = $(HOME)/.xinitrc
XRELOADRC = $(HOME)/.xreloadrc
XMODMAP = $(HOME)/.xmodmap
XDEFAULTS = $(HOME)/.Xdefaults

$(XINITRC): x/xinitrc
$(XRELOADRC): x/xreloadrc
$(XMODMAP): x/xmodmap
$(XDEFAULTS): x/Xdefaults

INSTALL += $(XINITRC) $(XRELOADRC) $(XMODMAP) $(XDEFAULTS)

# Install files

$(INSTALL):
	cp "$^" "$@"

all:: $(INSTALL)
