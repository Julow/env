{ stdenv, pkgs, fetchgit, ... }:

let
  src = fetchgit {
    url = "https://github.com/timbertson/opam2nix";
    rev = "d1d4cb15131bf45c9402aeff7df05dc78a92add7";
    sha256 = "1v65w65hiqjcgcgavjgcc6nbdjw3c1rcnpripp86ni5gmry548p1";
  };
in

pkgs.callPackage src {}
