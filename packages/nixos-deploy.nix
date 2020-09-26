{ pkgs, ... }:

let
  src = pkgs.fetchgit {
    url = "https://github.com/Julow/nixos-deploy";
    rev = "14c718fda984c58b2ac0af5bf2b637921a7239ff";
    sha256 = "0frr3z2v0177p90j4f07237qqnxcs9mc9lnq4df60fyjw05k7zpg";
  };

in

pkgs.callPackage src {}
