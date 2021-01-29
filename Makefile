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

XMONAD_BIN := $(shell which xmonad 2>/dev/null)

$(XMONAD_HS): xmonad/xmonad.hs $(XMONAD_BIN)
	cp "$<" "$@"
	xmonad --recompile

ifneq ($(XMONAD_BIN),)
all:: $(XMONAD_HS)
else
$(info XMonad disabled)
endif

# X

XDEFAULTS = $(HOME)/.Xdefaults
XPROFILE = $(HOME)/.xprofile

$(XDEFAULTS): x/Xdefaults
$(XPROFILE): x/xprofile

INSTALL += $(XDEFAULTS) $(XPROFILE)

# Git

GITCONFIG = $(HOME)/.gitconfig
GITIGNORE_GLOBAL = $(HOME)/.gitignore_global

$(GITCONFIG): $(abspath git/gitconfig)
	git config --global --replace-all include.path "$<" "$<"
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
BASH_PROFILE = $(HOME)/.bash_profile

$(INPUTRC): bash/inputrc

_bash.sh: $(sort $(wildcard bash/bashrc/*))
	bash bash/gen_bashrc.sh $^ > $@

_bash_profile.sh: $(sort $(wildcard bash/bash_profile/*))
	bash bash/gen_bashrc.sh $^ > $@

$(BASHRC) $(BASH_PROFILE):
	grep '$(SOURCE_LINE)' "$@" &>/dev/null || echo '$(SOURCE_LINE)' >> "$@"
	touch "$@"

$(BASHRC): _bash.sh
$(BASHRC): SOURCE_LINE = source "$(abspath _bash.sh)"

$(BASH_PROFILE): _bash_profile.sh
$(BASH_PROFILE): SOURCE_LINE = source "$(abspath _bash_profile.sh)"

INSTALL += $(INPUTRC)
all:: $(BASHRC) $(BASH_PROFILE)

# Htop

HTOPRC = $(HOME)/.config/htop/htoprc

$(HTOPRC): htop/htoprc

LINK += $(HTOPRC)

# Mpv

MPV_CONF = $(HOME)/.config/mpv/mpv.conf

$(MPV_CONF): mpv/mpv.conf

LINK += $(MPV_CONF)

# Vim

DOTVIM = $(HOME)/.vim

$(DOTVIM): vim $(wildcard vim/vimrc vim/ftplugin/*)
	[ -L "$@" ] || ! [ -d "$@" ]
	ln -sfn "$(abspath $<)" "$@"
	touch "$@"

all:: $(DOTVIM)

# External

update_vim:
	git subtree pull --squash -P vim/pack/plugins/start/fugitive "https://github.com/tpope/vim-fugitive" master
	git subtree pull --squash -P vim/pack/plugins/start/surround "https://github.com/tpope/vim-surround" master
	git subtree pull --squash -P vim/pack/plugins/start/commentary "https://github.com/tpope/vim-commentary" master
	git subtree pull --squash -P vim/pack/plugins/start/indent-object "https://github.com/michaeljsmith/vim-indent-object" master
	git subtree pull --squash -P vim/pack/plugins/start/argtextobj.vim "https://github.com/vim-scripts/argtextobj.vim" master
	git subtree pull --squash -P vim/pack/plugins/start/nix "https://github.com/LnL7/vim-nix" master
	git subtree pull --squash -P vim/pack/plugins/start/conflict3 "https://github.com/mkotha/conflict3" master
	git subtree pull --squash -P vim/pack/plugins/start/capnp "https://github.com/cstrahan/vim-capnp" master
	git subtree pull --squash -P vim/pack/plugins/start/markdown "https://github.com/plasticboy/vim-markdown" master
	git subtree pull --squash -P vim/pack/plugins/start/ctrlp "https://github.com/ctrlpvim/ctrlp.vim" master
	git subtree pull --squash -P vim/pack/plugins/start/json "https://github.com/elzr/vim-json" master
	git subtree pull --squash -P vim/pack/plugins/start/ripple "https://github.com/urbainvaes/vim-ripple" master
	git subtree pull --squash -P vim/pack/plugins/start/sneak "https://github.com/justinmk/vim-sneak" master
	git subtree pull --squash -P vim/pack/plugins/start/easy-align "https://github.com/junegunn/vim-easy-align" master
	git subtree pull --squash -P vim/pack/plugins/start/futhark "https://github.com/BeneCollyridam/futhark-vim" master
	git subtree pull --squash -P vim/pack/plugins/start/dirvish "https://github.com/justinmk/vim-dirvish" master
	git subtree pull --squash -P vim/pack/plugins/start/xdg_open "https://github.com/arp242/xdg_open.vim" master
	git subtree pull --squash -P vim/pack/plugins/start/gv "https://github.com/junegunn/gv.vim" master

update_vim_merlin:
	git fetch "https://github.com/ocaml/merlin"
	git rm -r vim/pack/plugins/start/merlin
	git read-tree --prefix=vim/pack/plugins/start/merlin -u FETCH_HEAD:vim/merlin
	git commit -m "Vim: Updated merlin"

.PHONY: update_vim update_vim_merlin

# Install files

# Exported for generating bashrc
export ENV_PATH := $(shell pwd)

$(INSTALL):
	@echo $@
	@mkdir -p "$(@D)"
	@sed 's#%ENV_PATH%#$(ENV_PATH)#' "$^" > "$@"

$(LINK):
	@echo $@
	@mkdir -p "$(@D)"
	@ln -sf "$(abspath $^)" "$@"

all:: $(INSTALL) $(LINK)
