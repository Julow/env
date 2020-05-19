{ stdenv, pkgs, fetchgit, ... }:

let
  src = fetchgit {
    url = "https://github.com/timbertson/opam2nix";
    rev = "764bcb4a4b6591b74f98c44f6b0cf76cb0db6fd8";
    sha256 = "0fxx8zb2n9jnn2xwbx9sblnhkd8myiahp6g909wynirl6yx3cymy";
  };
in

pkgs.callPackage src {
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_08;
}
