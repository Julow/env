PRIVATE_CONF_ARGS = --override-input private_conf "path:$$HOME/notes/setup"

build:
	nix flake lock ./vim
	time nixos-rebuild build --show-trace $(PRIVATE_CONF_ARGS) \
		--update-input vim_plugins --flake .
	nix path-info -Sh ./result

switch: build
	su -c "\
		nix-env -p /nix/var/nix/profiles/system --set $$(readlink ./result) && \
		result/bin/switch-to-configuration switch"

update:
	nix flake update $(PRIVATE_CONF_ARGS)

.PHONY: test build update
