#!/usr/bin/env bash

set -e

# Args: $config $@
build ()
{
  local config="$1"
  local -a nixpkgs
  shift
  if [[ -d nixpkgs ]]; then
    echo "Using nixpkgs at './nixpkgs'" >&2
    nixpkgs=(-I nixpkgs=./nixpkgs)
  fi
  echo "Building $config" >&2
  nix-build --no-out-link '<nixpkgs/nixos>' -I nixos-config="$config" "${nixpkgs[@]}" "$@"
}

case "$1" in
  deploy)
    target=${2?"Usage: $0 deploy <target>"}
    config=${3-configuration.nix}

    result=$(build "$config" -A system)
    echo "Built $result"

    echo "Uploading to $target"
    nix copy --no-check-sigs --to "$target" -s "$result"

    echo "Activating"
    ssh "$target" -- \
"nix-env --profile /nix/var/nix/profiles/system --set '$result' && \
/nix/var/nix/profiles/system/bin/switch-to-configuration switch"
    ;;

  vm)
    config=${2-configuration.nix}

    result=$(build "$config" -A vm)
    echo "Built $result"

    "$result"/bin/run-*-vm
    ;;

  trace)
    config=${2-configuration.nix}

    result=$(build "$config" -A system --show-trace)
    echo "Built $result"
    ;;

  *)
    cat <<EOF >&2
Usage: nixos-deploy { deploy <target> | vm | trace } [configuration.nix]

  If a directory named 'nixpkgs' exists in the current directory, it is used
  instead of the global nixpkgs channel.

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
