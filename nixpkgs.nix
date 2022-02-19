{
  nixpkgs = import (builtins.fetchTarball
    (builtins.fromJSON (builtins.readFile ./nixpkgs.json)));

  branch = "nixos-21.11";
}
