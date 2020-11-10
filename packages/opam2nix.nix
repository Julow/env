{ stdenv, pkgs, fetchgit, ... }:

let
  src = fetchgit {
    url = "https://github.com/timbertson/opam2nix";
    rev = "f6b45ed9e95e468f2971f7fee9dd5abbbc1d7577";
    sha256 = "0294jyqjnvvy9z79vxhkb2b3pwzg4wn7mg1bsb2j9m3rv2p6dxbs";
  };
in

pkgs.callPackage src {}
