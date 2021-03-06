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
	git subtree pull --squash -P vim/pack/plugins/start/markdown-folding "https://github.com/masukomi/vim-markdown-folding" master
	git subtree pull --squash -P vim/pack/plugins/start/xdg_open.vim "https://github.com/arp242/xdg_open.vim" master
	git subtree pull --squash -P vim/pack/plugins/start/tmux-focus-events" "any-term "https://github.com/Julow/vim-tmux-focus-events" any-term
	git subtree pull --squash -P vim/pack/plugins/start/mdnav "https://github.com/chmp/mdnav" master
	git subtree pull --squash -P vim/pack/plugins/start/conflict3 "https://github.com/mkotha/conflict3" master

update_fzf:
	git fetch "https://github.com/junegunn/fzf"
	git read-tree --prefix=bash/external/fzf -u FETCH_HEAD:shell
	git read-tree --prefix=vim/pack/plugins/start/fzf/plugin -u FETCH_HEAD:plugin

update_vim_merlin:
	git fetch "https://github.com/ocaml/merlin"
	git read-tree --prefix=vim/pack/plugins/start/merlin -u FETCH_HEAD:vim/merlin

.PHONY: update_vim update_fzf update_vim_merlin

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
