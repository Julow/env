all::
.PHONY: all

# Files to install, %ENV_PATH% will be substituted
# Rules must have a dependency, the source file, and no recipe
INSTALL =

# Files to link
# Rules must have a dependency, target file, and no recipe
LINK =

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

# Git

GITCONFIG = $(HOME)/.gitconfig
GITIGNORE_GLOBAL = $(HOME)/.gitignore_global

$(GITCONFIG): git/install_config.sh
	bash "$^"
	touch "$@"

$(GITIGNORE_GLOBAL): git/gitignore_global

INSTALL += $(GITIGNORE_GLOBAL)
all:: $(GITCONFIG)

# Tmux

TMUX_CONF = $(HOME)/.tmux.conf

$(TMUX_CONF): tmux/tmux.conf

INSTALL += $(TMUX_CONF)

# Dunst

DUNSTRC = $(HOME)/.config/dunst/dunstrc

$(DUNSTRC): dunst/dunstrc

INSTALL += $(DUNSTRC)

# Ack

ACKRC = $(HOME)/.ackrc

$(ACKRC): ack/ackrc

INSTALL += $(ACKRC)

# Bash

INPUTRC = $(HOME)/.inputrc
BASHRC = $(HOME)/.bashrc

$(INPUTRC): bash/inputrc

_bash.sh: $(sort $(wildcard bash/bashrc/*))
	bash bash/gen_bashrc.sh $^ > $@

BASH_SOURCE_LINE = source "$(abspath _bash.sh)"

$(BASHRC): _bash.sh
	grep "$(BASH_SOURCE_LINE)" "$@" &>/dev/null || echo "$(BASH_SOURCE_LINE)" >> "$@"
	touch "$@"

INSTALL += $(INPUTRC)
all:: $(BASHRC)

# Htop

HTOPRC = $(HOME)/.config/htop/htoprc

$(HTOPRC): htop/htoprc

LINK += $(HTOPRC)

# Install files

ENV_PATH := $(shell pwd)

$(INSTALL):
	@echo $@
	@mkdir -p "$(@D)"
	@sed 's#%ENV_PATH%#$(ENV_PATH)#' "$^" > "$@"

$(LINK):
	@echo $@
	@mkdir -p "$(@D)"
	@ln -sf "$(abspath $^)" "$@"

all:: $(INSTALL) $(LINK)
