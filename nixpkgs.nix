rec {
  src = builtins.fetchTarball
    (builtins.fromJSON (builtins.readFile ./nixpkgs.json));

  unpatched_nixpkgs = import src { };

  nixpkgs = import (unpatched_nixpkgs.applyPatches {
    name = "nixpkgs";
    inherit src patches;
  });

  patches = [ nixpkgs_patches/ocamlformat_0_20_0_and_0_20_1.patch ];

  branch = "nixos-21.11";
}
