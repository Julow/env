#!/usr/bin/env bash

set -e

case "$1" in
  deploy)
    target=${2?"Usage: $0 deploy <target>"}
    config=${3-configuration.nix}

    echo "Building $config"
    result=$(nix-build --no-out-link '<nixpkgs/nixos>' -A system -I nixos-config="$config")
    echo "Built $result"

    echo "Uploading to $target"
    nix-copy-closure --to --use-substitutes "$target" "$result"

    echo "Activating"
    ssh "$target" -- \
"nix-env --profile /nix/var/nix/profiles/system --set '$result' && \
/nix/var/nix/profiles/system/bin/switch-to-configuration switch"
    ;;

  vm)
    config=${2-configuration.nix}

    echo "Building $config"
    result=$(nix-build --no-out-link '<nixpkgs/nixos>' -A vm -I nixos-config="$config")
    echo "Built $result"

    "$result"/bin/run-*-vm
    ;;

  trace)
    config=${2-configuration.nix}

    echo "Building $config"
    result=$(nix-build --show-trace --no-out-link '<nixpkgs/nixos>' -A system -I nixos-config="$config")
    echo "Built $result"
    ;;

  *)
    cat <<EOF >&2
Usage: nixos-deploy { deploy <target> | vm | trace } [configuration.nix]

  deploy
    Build then deploy a NixOS system. <target> is an SSH uri, eg. root@remote.
    This is a thin wrapper around 'nix-build' and 'nix-copy-closure'. The system is
    built locally, uploaded to the remote machine then activated (switched to).

  vm
    Build a NixOS system and run it inside a qemu VM.

  trace
    Build a NixOS system with 'nix-build --show-trace' for debugging purpose.
EOF
    ;;
esac
