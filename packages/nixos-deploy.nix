{ pkgs, ... }:

let
  src = pkgs.fetchgit {
    url = "https://github.com/Julow/nixos-deploy";
    rev = "9332940f9051e87d47c6d89148451c259a32b8e5";
    sha256 = "1y7x9n7v309prr7090rrlqwlrs2m6pikl3flyhyv2b9zs2kzm5vx";
  };

in

pkgs.callPackage src {}
