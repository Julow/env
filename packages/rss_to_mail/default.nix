{ stdenv, pkgs, opam2nix, fetchgit, ... }:

let
  selection = opam2nix.build {
    src = fetchgit {
      url = "https://github.com/Julow/rss_to_mail.git";
      rev = "5556394d9c804c61905889aac403691b3823daf3";
      sha256 = "0awbwcw38pkvqzi9dbmx1ziamcdafnr9w6ws7nq8g2q7hb5ii2fh";
    };

    ocaml = pkgs.ocamlPackages.ocaml;

    selection = ./opam-selection.nix;
  };
in

selection.rss_to_mail
