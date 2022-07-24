all:
	# Would use '--use-remote-sudo' if it used 'su'
	nixos-rebuild build --flake .
	su -c "\
		nix-env -p /nix/var/nix/profiles/system --set $$(readlink ./result) && \
		result/bin/switch-to-configuration switch"

.PHONY: all
