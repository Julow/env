HOSTNAME ?= $(shell hostname)

all: host-$(HOSTNAME)

host-jules-work: config = host/work
host-jules-work: deploy-nixos
host-jules-pc: config = host/home
host-jules-pc: deploy-nixos

deploy-nixos:
	nixos-deploy deploy local $(config)

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
	git subtree pull --squash -P vim/pack/plugins/start/json "https://github.com/elzr/vim-json" master
	git subtree pull --squash -P vim/pack/plugins/start/sneak "https://github.com/justinmk/vim-sneak" master
	git subtree pull --squash -P vim/pack/plugins/start/easy-align "https://github.com/junegunn/vim-easy-align" master
	git subtree pull --squash -P vim/pack/plugins/start/futhark "https://github.com/BeneCollyridam/futhark-vim" master
	git subtree pull --squash -P vim/pack/plugins/start/dirvish "https://github.com/justinmk/vim-dirvish" master
	git subtree pull --squash -P vim/pack/plugins/start/xdg_open "https://github.com/arp242/xdg_open.vim" master
	git subtree pull --squash -P vim/pack/plugins/start/gv "https://github.com/junegunn/gv.vim" master
	git subtree pull --squash -P vim/pack/plugins/start/dwm "https://github.com/spolu/dwm.vim" master
	git subtree pull --squash -P vim/pack/plugins/start/markdown-folding "https://github.com/masukomi/vim-markdown-folding" master
	git subtree pull --squash -P vim/pack/plugins/start/ocaml "https://github.com/ocaml/vim-ocaml" master

update_vim_merlin:
	git fetch "https://github.com/ocaml/merlin"
	git rm -r vim/pack/plugins/start/merlin
	git read-tree --prefix=vim/pack/plugins/start/merlin -u FETCH_HEAD:vim/merlin
	git commit -m "Vim: Updated merlin"

.PHONY: all update_vim update_vim_merlin host-% home
