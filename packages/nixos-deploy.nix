{ pkgs, ... }:

let
  src = pkgs.fetchgit {
    url = "https://github.com/Julow/nixos-deploy";
    rev = "4e7ce58bbc80e00c5f4ac5e1fa4035b91ce9a752";
    sha256 = "0w5lz4q71b1axcdz6qglcg90f17zypk513n2fv235a7za7rq24ay";
  };

in

pkgs.callPackage src {}
