{ pkgs, ... }:

let
  src = pkgs.fetchgit {
    url = "https://github.com/Julow/nixos-deploy";
    rev = "8a33c8dc7f966a5d1e1cb62f9765d07ad86c1ca3";
    sha256 = "0qiljlbgslm2wdndi1wimxxv0qfpxczfl9vg3ajawldk8iqi0l7v";
  };

in

pkgs.callPackage src {}
