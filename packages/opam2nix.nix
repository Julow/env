{ stdenv, pkgs, fetchgit, ... }:

let
  src = fetchgit {
    url = "https://github.com/timbertson/opam2nix";
    rev = "baeffc1078309f9fdd4e0abd7e1a1f762e92bbe3";
    sha256 = "0asm16amhyfjppd92imrr4z5xq2f72f44phkc301l95gkwzp6v47";
  };
in

pkgs.callPackage src {}
